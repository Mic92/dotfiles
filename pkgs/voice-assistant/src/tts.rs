//! sherpa-onnx Kitten TTS with a reply cache, streamed sentence by
//! sentence into pw-play from background threads.

#[allow(
    non_upper_case_globals,
    non_camel_case_types,
    non_snake_case,
    dead_code,
    unsafe_op_in_unsafe_fn,
    clippy::all,
    clippy::pedantic,
    clippy::undocumented_unsafe_blocks
)]
mod sys {
    include!(concat!(env!("OUT_DIR"), "/sherpa_sys.rs"));
}

use std::env;
use std::ffi::{CString, c_void};
use std::fs;
use std::io::Write;
use std::iter;
use std::mem;
use std::path::{Path, PathBuf};
use std::process;
use std::process::{Command, Stdio};
use std::ptr::NonNull;
use std::slice;
use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::{Arc, Mutex, MutexGuard, PoisonError};
use std::thread;
use std::time::{Duration, Instant};

/// Owned sherpa-onnx TTS engine.
struct Engine(NonNull<sys::SherpaOnnxOfflineTts>);

// SAFETY: the engine has no thread affinity; it lives on the synthesis
// thread once created.
unsafe impl Send for Engine {}

impl Drop for Engine {
    fn drop(&mut self) {
        // SAFETY: created by SherpaOnnxCreateOfflineTts and destroyed exactly once.
        unsafe { sys::SherpaOnnxDestroyOfflineTts(self.0.as_ptr()) };
    }
}

/// Owned generated audio.
struct Generated(NonNull<sys::SherpaOnnxGeneratedAudio>);

impl Drop for Generated {
    fn drop(&mut self) {
        // SAFETY: returned by SherpaOnnxOfflineTtsGenerateWithConfig, freed once.
        unsafe { sys::SherpaOnnxDestroyOfflineTtsGeneratedAudio(self.0.as_ptr()) };
    }
}

/// Queued or playing utterances, and when the last one ended.
struct Playback {
    pending: usize,
    ended: Instant,
}

type Shared = Arc<Mutex<Playback>>;

fn lock(s: &Shared) -> MutexGuard<'_, Playback> {
    s.lock().unwrap_or_else(PoisonError::into_inner)
}

struct Job {
    text: String,
    cache: bool,
}

struct Utterance {
    rate: u32,
    chunks: Receiver<Vec<f32>>,
}

pub struct Tts {
    jobs: Sender<Job>,
    playback: Shared,
}

struct Synth {
    model_dir: PathBuf,
    voice: i32,
    speed: f32,
    engine: Option<Engine>,
}

/// English voices read German loan words the English way ("mate" as in
/// "buddy").
fn respell(text: &str) -> String {
    text.replace("Club-Mate", "Club Mahteh")
        .replace("Mate", "Mahteh")
}

fn cstr(p: &Path) -> Result<CString, String> {
    CString::new(p.as_os_str().as_encoded_bytes()).map_err(|e| e.to_string())
}

impl Tts {
    pub fn new(model_dir: PathBuf, voice: i32, speed: f32) -> Self {
        let synth = Synth {
            model_dir,
            voice: voice.max(0),
            speed: if speed.is_finite() && speed > 0.0 {
                speed
            } else {
                1.0
            },
            engine: None,
        };
        let playback: Shared = Arc::new(Mutex::new(Playback {
            pending: 0,
            ended: Instant::now(),
        }));
        let (jobs, job_rx) = mpsc::channel::<Job>();
        let (utter_tx, utter_rx) = mpsc::channel::<Utterance>();
        let shared = Arc::clone(&playback);
        thread::spawn(move || player(&utter_rx, &shared));
        let shared = Arc::clone(&playback);
        thread::spawn(move || synth.serve(&job_rx, &utter_tx, &shared));
        Tts { jobs, playback }
    }

    /// Queued after anything still being said; returns at once.
    pub fn say(&self, text: &str, cache: bool) {
        lock(&self.playback).pending += 1;
        let job = Job {
            text: respell(text),
            cache,
        };
        if self.jobs.send(job).is_err() {
            eprintln!("voice-assistant: TTS thread is gone");
            lock(&self.playback).pending -= 1;
        }
    }

    pub fn speaking(&self, echo: Duration) -> bool {
        let p = lock(&self.playback);
        p.pending > 0 || p.ended.elapsed() < echo
    }
}

impl Synth {
    fn serve(mut self, jobs: &Receiver<Job>, player: &Sender<Utterance>, shared: &Shared) {
        for job in jobs {
            if let Err(e) = self.speak(&job, player) {
                eprintln!("voice-assistant: {e}");
                lock(shared).pending -= 1;
            }
        }
    }

    /// Errors are returned only while the player doesn't own the job yet.
    fn speak(&mut self, job: &Job, player: &Sender<Utterance>) -> Result<(), String> {
        let cache = job.cache.then(|| self.cache_path(&job.text)).flatten();
        let hit = cache.as_deref().and_then(load_cached);
        let rate = match &hit {
            Some((_, rate)) => *rate,
            None => self.sample_rate()?,
        };
        let (tx, chunks) = mpsc::channel();
        player
            .send(Utterance { rate, chunks })
            .map_err(|_| "TTS player thread is gone")?;
        if let Some((samples, _)) = hit {
            let _ = tx.send(samples);
            return Ok(());
        }
        let mut all = Vec::new();
        let res = self.synthesize(&job.text, &mut |piece: &[f32]| {
            all.extend_from_slice(piece);
            let _ = tx.send(piece.to_vec());
        });
        drop(tx);
        match res {
            Err(e) => eprintln!("voice-assistant: {e}"),
            Ok(()) if all.is_empty() => eprintln!("voice-assistant: TTS returned no audio"),
            Ok(()) => {
                if let Some(p) = &cache {
                    store_cached(p, &all, rate);
                }
            }
        }
        Ok(())
    }

    fn engine(&mut self) -> Result<NonNull<sys::SherpaOnnxOfflineTts>, String> {
        if self.engine.is_none() {
            let d = &self.model_dir;
            // Kitten ships some sizes only quantized, others only in fp32.
            let model = ["model.onnx", "model.int8.onnx"]
                .iter()
                .map(|f| d.join(f))
                .find(|p| p.exists())
                .ok_or_else(|| format!("no Kitten model in {}", d.display()))?;
            let model = cstr(&model)?;
            let voices = cstr(&d.join("voices.bin"))?;
            let tokens = cstr(&d.join("tokens.txt"))?;
            let data = cstr(&d.join("espeak-ng-data"))?;
            let provider = c"cpu";
            // SAFETY: an all-zero config is the documented "unset" state; every
            // pointer stored in it refers to a CString that outlives the call,
            // and sherpa-onnx copies the paths during creation.
            let raw = unsafe {
                let mut cfg: sys::SherpaOnnxOfflineTtsConfig = mem::zeroed();
                cfg.model.kitten.model = model.as_ptr();
                cfg.model.kitten.voices = voices.as_ptr();
                cfg.model.kitten.tokens = tokens.as_ptr();
                cfg.model.kitten.data_dir = data.as_ptr();
                cfg.model.kitten.length_scale = 1.0;
                cfg.model.num_threads = 2;
                cfg.model.provider = provider.as_ptr();
                cfg.max_num_sentences = 1;
                sys::SherpaOnnxCreateOfflineTts(&raw const cfg)
            };
            let engine = NonNull::new(raw.cast_mut())
                .ok_or_else(|| format!("cannot load TTS model from {}", d.display()))?;
            self.engine = Some(Engine(engine));
        }
        self.engine
            .as_ref()
            .map(|e| e.0)
            .ok_or_else(|| "TTS engine missing".into())
    }

    fn sample_rate(&mut self) -> Result<u32, String> {
        let engine = self.engine()?;
        // SAFETY: a getter on a live engine.
        let rate = unsafe { sys::SherpaOnnxOfflineTtsSampleRate(engine.as_ptr()) };
        u32::try_from(rate)
            .ok()
            .filter(|r| *r > 0)
            .ok_or_else(|| "TTS reports an invalid sample rate".into())
    }

    /// Content address of a reply; the model path is a Nix store path, so a
    /// model update yields new keys.
    fn cache_path(&self, text: &str) -> Option<PathBuf> {
        let base = env::var_os("XDG_CACHE_HOME")
            .map(PathBuf::from)
            .or_else(|| env::var_os("HOME").map(|h| PathBuf::from(h).join(".cache")))?;
        let key = format!(
            "{}\0{}\0{}\0{}",
            self.model_dir.display(),
            self.voice,
            self.speed,
            text
        );
        // FNV-1a: stable across builds, unlike std's DefaultHasher.
        let hash = key.bytes().fold(0xcbf2_9ce4_8422_2325u64, |h, b| {
            (h ^ u64::from(b)).wrapping_mul(0x0100_0000_01b3)
        });
        Some(
            base.join("voice-assistant/tts")
                .join(format!("{hash:016x}.f32")),
        )
    }

    fn synthesize(&mut self, text: &str, piece: &mut dyn FnMut(&[f32])) -> Result<(), String> {
        unsafe extern "C" fn trampoline(
            samples: *const f32,
            n: i32,
            _progress: f32,
            arg: *mut c_void,
        ) -> i32 {
            // SAFETY: `arg` is the `&mut &mut dyn FnMut` passed below, alive for
            // the whole generate call; `samples` holds `n` floats during the
            // callback.
            unsafe {
                let piece = &mut *arg.cast::<&mut dyn FnMut(&[f32])>();
                if let Ok(n) = usize::try_from(n)
                    && n > 0
                    && !samples.is_null()
                {
                    piece(slice::from_raw_parts(samples, n));
                }
            }
            1
        }
        let (voice, speed) = (self.voice, self.speed);
        let engine = self.engine()?;
        let text = CString::new(text).map_err(|e| e.to_string())?;
        let mut piece: &mut dyn FnMut(&[f32]) = piece;
        // SAFETY: `engine` is live; `text`, `cfg` and `piece` outlive the call;
        // an all-zero generation config is the documented default apart from
        // the fields set.
        let raw = unsafe {
            let mut cfg: sys::SherpaOnnxGenerationConfig = mem::zeroed();
            cfg.sid = voice;
            cfg.speed = speed;
            cfg.silence_scale = 0.2;
            sys::SherpaOnnxOfflineTtsGenerateWithConfig(
                engine.as_ptr(),
                text.as_ptr(),
                &raw const cfg,
                Some(trampoline),
                (&raw mut piece).cast(),
            )
        };
        Generated(NonNull::new(raw.cast_mut()).ok_or("TTS generation failed")?);
        Ok(())
    }
}

/// Cache format: little-endian u32 sample rate followed by f32 samples.
/// Anything malformed counts as a miss.
fn load_cached(path: &Path) -> Option<(Vec<f32>, u32)> {
    let bytes = fs::read(path).ok()?;
    let (head, body) = bytes.split_at_checked(4)?;
    let rate = u32::from_le_bytes(head.try_into().ok()?);
    let (chunks, rest) = body.as_chunks::<4>();
    if rate == 0 || !rest.is_empty() || chunks.is_empty() {
        return None;
    }
    Some((
        chunks.iter().map(|b| f32::from_le_bytes(*b)).collect(),
        rate,
    ))
}

fn store_cached(path: &Path, samples: &[f32], rate: u32) {
    let mut bytes = rate.to_le_bytes().to_vec();
    bytes.extend(samples.iter().flat_map(|s| s.to_le_bytes()));
    // Write-then-rename so a crash never leaves a truncated entry.
    let tmp = path.with_extension(format!("tmp{}", process::id()));
    let res = path
        .parent()
        .map_or(Ok(()), fs::create_dir_all)
        .and_then(|()| fs::write(&tmp, &bytes))
        .and_then(|()| fs::rename(&tmp, path));
    if let Err(e) = res {
        let _ = fs::remove_file(&tmp);
        eprintln!("voice-assistant: caching TTS reply: {e}");
    }
}

fn player(utterances: &Receiver<Utterance>, shared: &Shared) {
    for u in utterances {
        if let Err(e) = play(&u) {
            eprintln!("voice-assistant: pw-play: {e}");
        }
        let mut p = lock(shared);
        p.pending = p.pending.saturating_sub(1);
        p.ended = Instant::now();
    }
}

fn play(u: &Utterance) -> Result<(), String> {
    let Ok(first) = u.chunks.recv() else {
        return Ok(());
    };
    let mut child = Command::new("pw-play")
        .args([
            "--raw",
            "--rate",
            &u.rate.to_string(),
            "--channels",
            "1",
            "--format",
            "f32",
            "-",
        ])
        .stdin(Stdio::piped())
        .spawn()
        .map_err(|e| e.to_string())?;
    let res = match child.stdin.take() {
        Some(mut stdin) => iter::once(first)
            .chain(u.chunks.iter())
            .try_for_each(|chunk| {
                let bytes: Vec<u8> = chunk.iter().flat_map(|s| s.to_le_bytes()).collect();
                stdin.write_all(&bytes).map_err(|e| e.to_string())
            }),
        None => Err("no stdin".into()),
    };
    // pw-play exits once the audio has drained.
    let _ = child.wait();
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn respells_club_mate() {
        assert_eq!(respell("Grab a Club-Mate."), "Grab a Club Mahteh.");
        assert_eq!(respell("Mate first."), "Mahteh first.");
        assert_eq!(respell("checkmate, my mate"), "checkmate, my mate");
    }
}
