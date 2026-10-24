//! Wake word -> VAD-bounded command recording -> whisper -> noctalia timer.

mod clock;
mod duration;
mod grammar;
mod http;
mod hush;
mod replay;
mod rescore;
mod skills;
mod store;
mod tts;
mod wakelog;
mod wakeword;
mod whisper;

use std::collections::VecDeque;
use std::env;
use std::error;
use std::fmt::Display;
use std::io;
use std::io::Cursor;
use std::io::Read;
use std::iter;
use std::mem;
use std::path::PathBuf;
use std::process;
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

const RATE_HZ: u32 = 16000;
const RATE: usize = RATE_HZ as usize;

struct Config {
    wake_model: PathBuf,
    threshold: f32,
    vad_model: PathBuf,
    whisper_model: PathBuf,
    language: String,
    threads: i32,
    sounds: PathBuf,
    tts_model: PathBuf,
    voice: i32,
    speed: f32,
    speak: bool,
    verbose: bool,
    stdin: bool,
    debug_dir: Option<PathBuf>,
    say: Option<String>,
    replay: bool,
    /// WAV files to transcribe without a prompt, then exit.
    transcribe: Vec<PathBuf>,
    /// Texts to run through the grammar, then exit.
    parse: Vec<String>,
}

fn default_path(build_env: Option<&str>) -> PathBuf {
    PathBuf::from(build_env.unwrap_or_default())
}

fn usage() -> ! {
    eprintln!(
        "usage: voice-assistant [--wake-model F] [--threshold P] [--vad-model F] [--whisper-model F]\n\
         \x20                      [--language L] [--threads N] [--sounds DIR] [--tts-model DIR]\n\
         \x20                      [--voice N] [--speed X] [--no-tts] [--verbose] [--stdin]\n\
         \x20                      [--debug-dir DIR] [--say TEXT] [--replay] [--parse TEXT|-]\n\
         --say speaks TEXT with the configured voice and exits.\n\
         --parse shows how TEXT (or each stdin line for -) is understood and exits.\n\
         --replay re-checks every logged utterance against the current build.\n\
         --debug-dir saves the audio around each wake word and command as WAV.\n\
         --stdin processes raw 16 kHz s16le mono from stdin instead of PipeWire."
    );
    process::exit(2)
}

fn parse_args() -> Config {
    let mut c = Config {
        wake_model: default_path(option_env!("VA_WAKE_MODEL")),
        threshold: 0.4,
        vad_model: default_path(option_env!("VA_VAD_MODEL")),
        whisper_model: default_path(option_env!("VA_WHISPER_MODEL")),
        language: "en".into(),
        threads: 4,
        sounds: default_path(option_env!("VA_SOUNDS")),
        tts_model: default_path(option_env!("VA_TTS_MODEL")),
        voice: 5,
        speed: 1.0,
        speak: true,
        verbose: false,
        stdin: false,
        debug_dir: None,
        say: None,
        replay: false,
        transcribe: Vec::new(),
        parse: Vec::new(),
    };
    let mut args = env::args().skip(1);
    while let Some(a) = args.next() {
        let mut val = || args.next().unwrap_or_else(|| usage());
        match a.as_str() {
            "--wake-model" => c.wake_model = val().into(),
            "--threshold" => c.threshold = val().parse().unwrap_or_else(|_| usage()),
            "--vad-model" => c.vad_model = val().into(),
            "--whisper-model" => c.whisper_model = val().into(),
            "--language" => c.language = val(),
            "--threads" => c.threads = val().parse().unwrap_or_else(|_| usage()),
            "--sounds" => c.sounds = val().into(),
            "--tts-model" => c.tts_model = val().into(),
            "--voice" => c.voice = val().parse().unwrap_or_else(|_| usage()),
            "--speed" => c.speed = val().parse().unwrap_or_else(|_| usage()),
            "--no-tts" => c.speak = false,
            "--verbose" => c.verbose = true,
            "--stdin" => c.stdin = true,
            "--debug-dir" => c.debug_dir = Some(val().into()),
            "--say" => c.say = Some(val()),
            "--replay" => c.replay = true,
            "--transcribe" => c.transcribe.push(val().into()),
            "--parse" => c.parse.push(val()),
            _ => usage(),
        }
    }
    c
}

fn die(msg: impl Display) -> ! {
    eprintln!("voice-assistant: {msg}");
    process::exit(1)
}

enum State {
    Idle,
    Listening {
        /// Samples still to discard: our own wake chime.
        skip: usize,
        /// The wake word as heard, to be confirmed by whisper; empty when
        /// listening for an answer.
        wake: Vec<f32>,
        audio: Vec<f32>,
        /// Transcript made at the last pause; dropped when speech resumes.
        guess: Option<Result<String, String>>,
        speech_frames: usize,
        silent_frames: usize,
    },
}

struct Assistant {
    cfg: Config,
    wake: wakeword::WakeWord,
    vad: whisper::Vad,
    whisper: whisper::Whisper,
    tts: tts::Tts,
    state: State,
    /// The mic hears our own replies; ignore it until playback has ended.
    speaking: bool,
    /// Answer to a question we asked, heard without a wake word.
    follow_up: Option<skills::FollowUp>,
    /// Players paused for this conversation.
    hushed: Option<hush::Hushed>,
    /// Resume them once the reply has been spoken.
    resume_after_speech: bool,
    /// Whisper prompt for the answer to that question.
    follow_prompt: Option<&'static str>,
    /// Misunderstandings in a row.
    misses: u32,
    /// Ignoring everything but "wake up"; `None` inside means until then.
    asleep: Option<Option<Instant>>,
    /// For "repeat that".
    last_speech: String,
    /// An action running in the background.
    pending: Option<mpsc::Receiver<(skills::Action, skills::Reply)>>,
    /// For "and tomorrow?"; forgotten after `CARRY_OVER`.
    previous: Option<(skills::Action, Instant)>,
    wakelog: wakelog::WakeLog,
    /// Last seconds of audio, for the wake word log and --debug-dir.
    history: VecDeque<i16>,
    started: Instant,
    dumps: u32,
}

// Listening budget, in 32 ms VAD frames / seconds of audio.
const END_SILENCE_FRAMES: usize = 25;
/// Start transcribing at this much silence already; most pauses this long
/// are the end.
const GUESS_SILENCE_FRAMES: usize = 10;
/// Chatter pauses all the time; don't keep guessing at it.
const GUESS_MAX_SECS: usize = 5;
const MAX_LISTEN_SECS: usize = 8;
const NO_SPEECH_SECS: usize = 4;
const SPEECH_PROB: f32 = 0.5;
/// Audio before the trigger handed to whisper to confirm the wake word.
const WAKE_CLIP: usize = 2 * RATE;
/// pw-play startup plus room echo.
const ECHO: Duration = Duration::from_millis(700);
const CARRY_OVER: Duration = Duration::from_secs(120);
/// About 0.3 s; a click or the tail of a chime is shorter than any command.
const MIN_SPEECH_FRAMES: usize = 10;
/// The wake chime (0.36 s) plus pw-play startup; the mic must not hear it.
const CHIME_SAMPLES: usize = RATE * 6 / 10;

impl Assistant {
    fn sound(&self, name: &str) {
        if self.cfg.stdin {
            return;
        }
        let path = self.cfg.sounds.join(format!("{name}.ogg"));
        skills::spawn("pw-play", &[&path.to_string_lossy()]);
    }

    fn log(&self, msg: &str) {
        if self.cfg.verbose || self.cfg.stdin {
            println!("{:8.2} {msg}", self.started.elapsed().as_secs_f32());
        }
    }

    fn reset_wake(&mut self) {
        if let Err(e) = self.wake.reset() {
            eprintln!("voice-assistant: resetting wake word: {e}");
        }
    }

    fn dump(&mut self, kind: &str, samples: &[i16]) {
        let Some(dir) = &self.cfg.debug_dir else {
            return;
        };
        self.dumps += 1;
        let path = dir.join(format!("{:04}-{kind}.wav", self.dumps));
        match store::write_wav(&path, samples) {
            Ok(()) => self.log(&format!("saved {}", path.display())),
            Err(e) => eprintln!("voice-assistant: {}: {e}", path.display()),
        }
    }

    /// Log every utterance for `--replay`; misses keep their audio as
    /// material for new skills and test cases.
    fn log_heard(&self, text: &str, action: Option<&skills::Action>, audio: &[f32]) {
        if self.cfg.stdin {
            return;
        }
        let Some(dir) = store::state_dir() else {
            return;
        };
        let stamp = store::unix_ms() / 1000;
        let wav = action
            .is_none()
            .then(|| dir.join("misses").join(format!("{stamp}.wav")));
        let line = store::tsv(&[
            &stamp.to_string(),
            text,
            &action.map(replay::kind).unwrap_or_default(),
            &wav.as_ref()
                .map(|w| w.display().to_string())
                .unwrap_or_default(),
        ]);
        let res = wav
            .as_ref()
            .map_or(Ok(()), |w| store::write_wav(w, &store::to_pcm(audio)))
            .and_then(|()| store::append(&dir.join("heard.tsv"), &line));
        if let Err(e) = res {
            eprintln!("voice-assistant: saving miss: {e}");
        }
    }

    /// The reply queues behind any filler line still playing.
    fn poll(&mut self) {
        if let Some(rx) = &self.pending {
            match rx.try_recv() {
                Ok((action, reply)) => {
                    self.pending = None;
                    self.finish(action, reply);
                }
                Err(mpsc::TryRecvError::Empty) => {}
                Err(mpsc::TryRecvError::Disconnected) => {
                    eprintln!("voice-assistant: action thread died");
                    self.pending = None;
                    self.sound("error");
                }
            }
        }
    }

    /// Returns true when a command finished and buffered audio is stale.
    fn feed(&mut self, samples: &[i16]) -> bool {
        self.poll();
        self.history.extend(samples);
        let excess = self.history.len().saturating_sub(3 * RATE);
        self.history.drain(..excess);
        if self.speaking {
            if self.tts.speaking(ECHO) {
                return false;
            }
            self.speaking = false;
            self.reset_wake();
            if self.follow_up.is_some() {
                self.start_listening(Vec::new());
            } else if mem::take(&mut self.resume_after_speech) {
                self.unhush();
            }
        }
        match &mut self.state {
            State::Idle => {
                if let Some(p) = self.wake.feed(samples) {
                    self.log(&format!("wake word p={p:.2}"));
                    self.wake.take_peak();
                    let before: Vec<i16> = self.history.iter().copied().collect();
                    self.wakelog.triggered(p, &before);
                    self.dump("wake", &before);
                    let wake = before[before.len().saturating_sub(WAKE_CLIP)..]
                        .iter()
                        .map(|&s| f32::from(s) / 32768.0)
                        .collect();
                    self.start_listening(wake);
                } else {
                    let peak = self.wake.take_peak();
                    let history = &self.history;
                    self.wakelog
                        .observe(peak, || history.iter().copied().collect());
                }
                false
            }
            State::Listening {
                skip,
                wake,
                audio,
                speech_frames,
                silent_frames,
                guess,
            } => {
                let dropped = (*skip).min(samples.len());
                *skip -= dropped;
                let f: Vec<f32> = samples[dropped..]
                    .iter()
                    .map(|&s| f32::from(s) / 32768.0)
                    .collect();
                audio.extend_from_slice(&f);
                for p in self.vad.feed(&f) {
                    if p > SPEECH_PROB {
                        *speech_frames += 1;
                        *silent_frames = 0;
                        *guess = None;
                    } else {
                        *silent_frames += 1;
                    }
                }
                let speech = *speech_frames >= MIN_SPEECH_FRAMES;
                let secs = audio.len() / RATE;
                let ended = speech && *silent_frames >= END_SILENCE_FRAMES;
                let done = ended || secs >= MAX_LISTEN_SECS || (!speech && secs >= NO_SPEECH_SECS);
                if !done {
                    if speech
                        && *silent_frames >= GUESS_SILENCE_FRAMES
                        && guess.is_none()
                        && audio.len() < GUESS_MAX_SECS * RATE
                    {
                        let (wake, audio) = (wake.clone(), audio.clone());
                        let early = self.transcribe_command(&wake, &audio);
                        if let State::Listening { guess, .. } = &mut self.state {
                            *guess = Some(early);
                        }
                    }
                    return false;
                }
                let State::Listening {
                    wake,
                    audio,
                    speech_frames,
                    guess,
                    ..
                } = mem::replace(&mut self.state, State::Idle)
                else {
                    unreachable!()
                };
                if speech_frames >= MIN_SPEECH_FRAMES {
                    if self.cfg.debug_dir.is_some() {
                        self.dump("command", &store::to_pcm(&audio));
                    }
                    self.handle(&wake, &audio, !ended, guess);
                } else {
                    self.log("no speech");
                    self.wakelog.outcome("no-speech", "");
                    self.follow_up = None;
                    self.sound("cancel");
                    self.unhush();
                }
                self.reset_wake();
                true
            }
        }
    }

    fn say(&mut self, text: &str, cache: bool) {
        if !self.cfg.speak || self.cfg.stdin {
            return;
        }
        self.tts.say(text, cache);
        self.speaking = true;
    }

    /// `wake` (empty for answers) is prepended so whisper can confirm it.
    fn transcribe_command(&mut self, wake: &[f32], audio: &[f32]) -> Result<String, String> {
        let prompt = self.follow_up.as_ref().and(self.follow_prompt);
        // The gap keeps whisper from gluing the wake word to the command.
        let gap = if wake.is_empty() { 0 } else { RATE / 5 };
        let joined: Vec<f32> = wake
            .iter()
            .copied()
            .chain(iter::repeat_n(0.0, gap))
            .chain(audio.iter().copied())
            .collect();
        self.whisper.transcribe(&joined, prompt).map(|t| t.text)
    }

    /// `cut_off`: the recording hit the time limit without a pause, which is
    /// what background chatter looks like. `guess`: the transcript of the
    /// same audio started at the first pause.
    fn handle(
        &mut self,
        wake: &[f32],
        audio: &[f32],
        cut_off: bool,
        guess: Option<Result<String, String>>,
    ) {
        self.sound("processing");
        let start = Instant::now();
        let early = guess.is_some();
        let raw = match guess.unwrap_or_else(|| self.transcribe_command(wake, audio)) {
            Ok(t) => t,
            Err(e) => {
                eprintln!("voice-assistant: {e}");
                self.wakelog.outcome("error", "");
                self.sound("error");
                self.end_conversation(false);
                return;
            }
        };
        self.log(&format!(
            "heard {:?} ({:.1}s audio, {})",
            raw,
            audio.len() as f32 / RATE as f32,
            if early {
                "transcribed at the first pause".to_owned()
            } else {
                format!("{:.2}s transcription", start.elapsed().as_secs_f32())
            }
        ));
        let (said_before, text) = if wake.is_empty() {
            ("", raw.as_str())
        } else if let Some(rest) = skills::strip_wake_word(&raw) {
            // Just "Alexa?" is a command of its own.
            if rest.is_empty() {
                ("", raw.as_str())
            } else {
                (&raw[..raw.len() - rest.len()], rest)
            }
        } else {
            // The detector fires on "Alexandra", "a lecture" and music;
            // whisper hearing no "Alexa" in the same audio settles it.
            self.log(&format!("no wake word in {raw:?}, ignoring"));
            self.wakelog.outcome("false", &raw);
            self.end_conversation(false);
            return;
        };
        let (said_before, text) = (said_before.to_owned(), text.to_owned());
        let mut ctx = skills::Context::local();
        ctx.previous = self
            .previous
            .as_ref()
            .filter(|(_, at)| at.elapsed() < CARRY_OVER)
            .map(|(a, _)| a.clone());
        if let Some(Some(end)) = self.asleep
            && Instant::now() >= end
        {
            self.asleep = None;
        }
        if self.asleep.is_some()
            && skills::parse_at(&text, &ctx) != Some(skills::Action::Listen(true))
        {
            // Only "wake up" gets through; the rest stays quiet.
            self.log("asleep, ignoring");
            self.wakelog.outcome("asleep", &text);
            self.sound("cancel");
            self.end_conversation(false);
            return;
        }
        // Chatter that ran into the time limit answers no question.
        let answer = self
            .follow_up
            .take()
            .filter(|_| !cut_off)
            .and_then(|f| f(&text, &ctx));
        if text.is_empty() {
            self.log("whisper heard no speech");
            self.wakelog.outcome("no-speech", "");
            self.follow_up = None;
            self.sound("cancel");
            self.end_conversation(false);
            return;
        }
        // Rescoring against the audio comes before the catch-all web
        // questions, which would happily take any mangled command.
        let strict = answer.or_else(|| skills::parse_tier(&text, &ctx, false));
        if strict.is_none() && cut_off {
            // Whisper makes up the same few phrases ("What's the number?")
            // for endless noise; only an exact command gets through.
            self.log(&format!(
                "no pause in {}s, treating {text:?} as noise",
                MAX_LISTEN_SECS
            ));
            self.log_heard(&text, None, audio);
            self.wakelog.outcome("noise", &text);
            self.sound("cancel");
            self.end_conversation(false);
            return;
        }
        let action = strict
            .or_else(|| self.rescore(&said_before, &text, &ctx))
            .or_else(|| skills::parse_tier(&text, &ctx, true));
        let Some(action) = action else {
            self.misses += 1;
            let speech = skills::not_understood(&text, self.misses);
            self.log_heard(&text, None, audio);
            self.wakelog.outcome("miss", &text);
            self.last_speech.clone_from(&speech);
            self.notify("Didn't understand", &speech);
            self.sound("error");
            // Quotes the transcript, so caching would only fill the disk.
            self.say(&speech, false);
            self.end_conversation(false);
            return;
        };
        self.log(&format!("action {action:?}"));
        self.log_heard(&text, Some(&action), &[]);
        self.wakelog.outcome("command", &text);
        if self.cfg.stdin {
            return;
        }
        self.misses = 0;
        if let Some(line) = skills::filler(&action) {
            self.say(&line, true);
        }
        // Network lookups take seconds; the wake word keeps listening meanwhile.
        let (tx, rx) = mpsc::channel();
        thread::spawn(move || {
            let reply = skills::run(&action, &ctx);
            let _ = tx.send((action, reply));
        });
        self.pending = Some(rx);
    }

    fn rescore(
        &mut self,
        said_before: &str,
        text: &str,
        ctx: &skills::Context,
    ) -> Option<skills::Action> {
        if self.cfg.stdin {
            return None;
        }
        let r = rescore::run(&mut self.whisper, said_before, text, ctx)?;
        self.log(&r.to_string());
        r.accepted.then_some(r.action)
    }

    fn finish(&mut self, action: skills::Action, mut reply: skills::Reply) {
        // Media commands decide about the music themselves.
        let media_handled =
            matches!(&action, skills::Action::Media(m) if *m != skills::media::Media::NowPlaying);
        if action == skills::Action::Repeat {
            reply.speech = if self.last_speech.is_empty() {
                "I haven't said anything yet. Shy?".into()
            } else {
                self.last_speech.clone()
            };
        }
        match &action {
            skills::Action::Listen(on) => self.asleep = (!*on).then_some(None),
            skills::Action::Nap(secs) => {
                self.asleep = Some(Some(Instant::now() + Duration::from_secs(u64::from(*secs))));
            }
            _ => {}
        }
        let title = if reply.title.is_empty() {
            "Assistant"
        } else {
            &reply.title
        };
        let body = [reply.body.as_str(), reply.speech.as_str()]
            .into_iter()
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
            .join("\n");
        self.notify(title, &body);
        self.sound(if action == skills::Action::Dismiss {
            "cancel"
        } else {
            "success"
        });
        if action == skills::Action::Voice(true) {
            self.cfg.speak = true;
        }
        self.say(&reply.speech, reply.cache);
        self.last_speech.clone_from(&reply.speech);
        if action == skills::Action::Voice(false) {
            self.cfg.speak = false;
        }
        if !matches!(
            action,
            skills::Action::Repeat
                | skills::Action::Dismiss
                | skills::Action::Say(_)
                | skills::Action::Voice(_)
                | skills::Action::Listen(_)
        ) {
            self.previous = Some((action, Instant::now()));
        }
        self.follow_up = reply.follow_up;
        self.follow_prompt = reply.expect;
        if self.follow_up.is_none() {
            self.end_conversation(media_handled);
        }
        // Without a spoken question there is no playback to wait for.
        if self.follow_up.is_some() && !self.speaking {
            self.start_listening(Vec::new());
        }
    }

    /// The conversation is over: resume the music, after any reply.
    fn end_conversation(&mut self, leave_paused: bool) {
        if leave_paused {
            self.hushed = None;
        } else if self.speaking {
            self.resume_after_speech = true;
        } else {
            self.unhush();
        }
    }

    fn unhush(&mut self) {
        if let Some(h) = self.hushed.take() {
            h.resume();
        }
    }

    fn start_listening(&mut self, wake: Vec<f32>) {
        if self.hushed.is_none() && !self.cfg.stdin {
            self.hushed = Some(hush::hush());
        }
        self.sound("wake");
        self.vad.reset();
        self.state = State::Listening {
            skip: if self.cfg.stdin { 0 } else { CHIME_SAMPLES },
            wake,
            audio: Vec::with_capacity(MAX_LISTEN_SECS * RATE),
            guess: None,
            speech_frames: 0,
            silent_frames: 0,
        };
    }

    fn notify(&self, title: &str, body: &str) {
        if !self.cfg.stdin {
            skills::spawn(
                "notify-send",
                &["-a", "Voice assistant", "-t", "4000", title, body],
            );
        }
    }
}

fn run_worker(mut a: Assistant, rx: &mpsc::Receiver<Vec<i16>>) {
    while let Ok(chunk) = rx.recv() {
        if a.feed(&chunk) {
            // Audio captured while transcribing belongs to the old command.
            while rx.try_recv().is_ok() {}
        }
    }
}

fn run_pipewire(tx: mpsc::Sender<Vec<i16>>) -> Result<(), Box<dyn error::Error>> {
    use pipewire as pw;
    use pw::spa;
    use pw::spa::pod::Pod;

    pw::init();
    let mainloop = pw::main_loop::MainLoopRc::new(None)?;
    let context = pw::context::ContextRc::new(&mainloop, None)?;
    let core = context.connect_rc(None)?;
    let props = pw::properties::properties! {
        *pw::keys::MEDIA_TYPE => "Audio",
        *pw::keys::MEDIA_CATEGORY => "Capture",
        *pw::keys::MEDIA_ROLE => "Communication",
        *pw::keys::APP_NAME => "voice-assistant",
        *pw::keys::NODE_LATENCY => "1600/16000",
        // speech features stop at 7.5 kHz; the default resampler quality is wasted here
        "resample.quality" => "0",
    };
    let stream = pw::stream::StreamBox::new(&core, "voice-assistant", props)?;
    let _listener = stream
        .add_local_listener_with_user_data(tx)
        .process(|stream, tx| {
            let Some(mut buffer) = stream.dequeue_buffer() else {
                return;
            };
            let datas = buffer.datas_mut();
            let Some(data) = datas.first_mut() else {
                return;
            };
            let size = data.chunk().size() as usize;
            if let Some(bytes) = data.data() {
                let n = size.min(bytes.len());
                let samples = bytes[..n]
                    .as_chunks::<2>()
                    .0
                    .iter()
                    .map(|b| i16::from_le_bytes(*b))
                    .collect();
                let _ = tx.send(samples);
            }
        })
        .register()?;

    let mut info = spa::param::audio::AudioInfoRaw::new();
    info.set_format(spa::param::audio::AudioFormat::S16LE);
    info.set_rate(RATE_HZ);
    info.set_channels(1);
    let obj = spa::pod::Object {
        type_: spa::utils::SpaTypes::ObjectParamFormat.as_raw(),
        id: spa::param::ParamType::EnumFormat.as_raw(),
        properties: info.into(),
    };
    let values: Vec<u8> = spa::pod::serialize::PodSerializer::serialize(
        Cursor::new(Vec::new()),
        &spa::pod::Value::Object(obj),
    )
    .map_err(|e| format!("serialize audio format: {e:?}"))?
    .0
    .into_inner();
    let mut params = [Pod::from_bytes(&values).ok_or("invalid audio format pod")?];
    stream.connect(
        spa::utils::Direction::Input,
        None,
        pw::stream::StreamFlags::AUTOCONNECT | pw::stream::StreamFlags::MAP_BUFFERS,
        &mut params,
    )?;
    mainloop.run();
    Ok(())
}

/// Exit code 1 when any text matched nothing, so it can gate scripts.
fn parse_texts(texts: &[String]) -> i32 {
    let ctx = skills::Context::local();
    let mut texts: Vec<String> = texts.to_vec();
    if let Some(i) = texts.iter().position(|t| t == "-") {
        let lines = io::stdin().lines().map_while(Result::ok);
        texts.splice(i..=i, lines.filter(|l| !l.trim().is_empty()));
    }
    let mut missed = false;
    for t in &texts {
        let (tier, action) = match skills::parse_tier(t, &ctx, false) {
            Some(a) => ("intent  ", Some(a)),
            None => ("fallback", skills::parse_tier(t, &ctx, true)),
        };
        match action {
            Some(a) => println!("{tier}  {t}\n          {a:?}"),
            None => {
                missed = true;
                println!("MISS      {t}");
            }
        }
    }
    i32::from(missed)
}

fn main() {
    let cfg = parse_args();
    if let Some(text) = &cfg.say {
        let tts = tts::Tts::new(cfg.tts_model.clone(), cfg.voice, cfg.speed);
        tts.say(text, false);
        while tts.speaking(Duration::ZERO) {
            thread::sleep(Duration::from_millis(50));
        }
        return;
    }
    if !cfg.parse.is_empty() {
        process::exit(parse_texts(&cfg.parse));
    }
    whisper::init();
    let wake =
        wakeword::WakeWord::new(&cfg.wake_model, cfg.threshold, 5).unwrap_or_else(|e| die(e));
    let vad = whisper::Vad::new(&cfg.vad_model).unwrap_or_else(|e| die(e));
    let whisper = whisper::Whisper::new(
        cfg.whisper_model.clone(),
        &cfg.language,
        &skills::prompt(),
        cfg.threads,
    )
    .unwrap_or_else(|e| die(e));
    if !cfg.transcribe.is_empty() {
        let mut whisper = whisper;
        for f in &cfg.transcribe {
            match replay::read_wav(f).map(|a| whisper.transcribe(&a, Some(""))) {
                Some(Ok(t)) => println!("{}\t{}", f.display(), t.text),
                Some(Err(e)) => eprintln!("voice-assistant: {}: {e}", f.display()),
                None => eprintln!("voice-assistant: {}: unreadable", f.display()),
            }
        }
        process::exit(0);
    }
    if cfg.replay {
        let mut whisper = whisper;
        let log = store::state_dir()
            .unwrap_or_else(|| die("no state directory"))
            .join("heard.tsv");
        process::exit(replay::run(&log, Some(&mut whisper)));
    }
    let tts = tts::Tts::new(cfg.tts_model.clone(), cfg.voice, cfg.speed);
    let stdin = cfg.stdin;
    let threshold = cfg.threshold;
    let mut assistant = Assistant {
        cfg,
        wake,
        vad,
        whisper,
        tts,
        state: State::Idle,
        speaking: false,
        follow_up: None,
        follow_prompt: None,
        hushed: None,
        resume_after_speech: false,
        misses: 0,
        asleep: None,
        last_speech: String::new(),
        previous: None,
        pending: None,
        history: VecDeque::new(),
        started: Instant::now(),
        dumps: 0,
        wakelog: wakelog::WakeLog::new(if stdin { None } else { store::state_dir() }, threshold),
    };

    if stdin {
        let mut raw = Vec::new();
        if let Err(e) = io::stdin().read_to_end(&mut raw) {
            die(format!("reading stdin: {e}"));
        }
        let samples: Vec<i16> = raw
            .as_chunks::<2>()
            .0
            .iter()
            .map(|b| i16::from_le_bytes(*b))
            .collect();
        for chunk in samples.chunks(1600) {
            assistant.feed(chunk);
        }
        // Flush a command that ran into the end of the input.
        assistant.feed(&vec![0; MAX_LISTEN_SECS * RATE]);
        return;
    }

    let (tx, rx) = mpsc::channel();
    let worker = thread::spawn(move || run_worker(assistant, &rx));
    if let Err(e) = run_pipewire(tx) {
        die(format!("pipewire: {e}"));
    }
    let _ = worker.join();
}
