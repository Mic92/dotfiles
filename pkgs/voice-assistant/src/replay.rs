//! `--replay`: re-run everything logged in heard.tsv through the current
//! grammar (and speech recogniser, where audio was kept) and report what
//! changed.

use crate::asr::Asr;
use crate::brain::{self, Parses};
use crate::classify::Classifier;
use crate::rescore;
use crate::skills::{self, Context};
use std::fs;
use std::path::Path;

/// "Info(Weather", "Fun(Joke)": the action without its slot values, which
/// may legitimately differ between runs (timestamps, random picks).
pub fn kind(a: &skills::Action) -> String {
    kind_of(&format!("{a:?}"))
}

/// `kind` of an action as `Debug` prints it.
pub fn kind_of(d: &str) -> String {
    let mut end = d.len();
    for (n, (i, _)) in d.match_indices(['(', ' ', '{', '"']).enumerate() {
        if n == 1 || d[i..].starts_with([' ', '{', '"']) {
            end = i;
            break;
        }
    }
    d[..end].to_owned()
}

/// `--golden`: what the grammar alone makes of `text`, from the regular
/// intents and from the fallback ones, as `text\tintent\tfallback` with
/// `-` for nothing. The clock
/// is fixed so dates don't drift between runs.
pub fn golden_line(text: &str) -> String {
    let ctx = Context::at(9 * 3600);
    let show = |a: Option<skills::Action>| a.map_or_else(|| "-".to_owned(), |a| format!("{a:?}"));
    format!(
        "{text}\t{}\t{}",
        show(skills::parse_tier(text, &ctx, false)),
        show(skills::parse_tier(text, &ctx, true))
    )
}

/// Which brain reads the replayed sentences.
pub enum Reader {
    Rust,
    Brain(brain::Shared),
}

impl Reader {
    pub fn read(&self, texts: &[String], ctx: &Context) -> Vec<Parses> {
        match self {
            Reader::Rust => texts
                .iter()
                .map(|t| {
                    let show =
                        |fallback| skills::parse_tier(t, ctx, fallback).map(|a| format!("{a:?}"));
                    Parses {
                        strict: show(false),
                        fallback: show(true),
                    }
                })
                .collect(),
            Reader::Brain(b) => b.parses(texts, ctx),
        }
    }

    fn sound_alikes(&self) -> &skills::SoundAlikes {
        match self {
            Reader::Rust => &skills::NAMED,
            Reader::Brain(b) => b.sound_alikes(),
        }
    }

    /// The kind of action the classifier stands for, if sure.
    fn classify(&self, c: &mut Classifier, text: &str, ctx: &Context) -> Option<String> {
        match self {
            Reader::Rust => {
                let hit = c.classify(text, ctx).ok()??;
                println!("        {hit}");
                hit.action.filter(|_| hit.accepted).map(|a| kind(&a))
            }
            Reader::Brain(b) => {
                let n = c.nearest(text).ok()??;
                let (action, accepted) = b.classified(text, &n, ctx).ok()?;
                println!(
                    "        classified as {} ({:.3}, lead {:.3}, like {:?}), {}",
                    action
                        .clone()
                        .unwrap_or_else(|| format!("{} without slots", n.intent)),
                    n.score,
                    n.lead,
                    n.example,
                    if accepted { "accepted" } else { "rejected" }
                );
                action.filter(|_| accepted).map(|a| kind_of(&a))
            }
        }
    }

    fn read_one(&self, text: &str, ctx: &Context) -> Parses {
        self.read(&[text.to_owned()], ctx).pop().unwrap_or_default()
    }
}

/// 16-bit mono PCM from a WAV file, as written by `write_wav`.
pub fn read_wav(path: &Path) -> Option<Vec<f32>> {
    let bytes = fs::read(path).ok()?;
    let at = bytes.windows(4).position(|w| w == b"data")?;
    let data = bytes.get(at + 8..)?;
    Some(
        data.as_chunks::<2>()
            .0
            .iter()
            .map(|b| f32::from(i16::from_le_bytes(*b)) / 32768.0)
            .collect(),
    )
}

#[derive(Debug, PartialEq)]
enum Outcome {
    Same,
    Fixed,
    Broken,
    Changed,
}

fn compare(old: &str, new: Option<&str>) -> Outcome {
    match (old.is_empty(), new) {
        (true, None) => Outcome::Same,
        (true, Some(_)) => Outcome::Fixed,
        (false, None) => Outcome::Broken,
        (false, Some(n)) if n == old => Outcome::Same,
        (false, Some(_)) => Outcome::Changed,
    }
}

/// Returns the process exit code: 1 when something that used to work broke.
pub fn run(
    log: &Path,
    reader: &Reader,
    mut asr: Option<&mut Asr>,
    mut classifier: Option<&mut Classifier>,
) -> i32 {
    let Ok(text) = fs::read_to_string(log) else {
        eprintln!("voice-assistant: nothing to replay in {}", log.display());
        return 1;
    };
    let ctx = Context::local();
    let (mut same, mut fixed, mut broken, mut changed) = (0, 0, 0, 0);
    for line in text.lines() {
        let mut f = line.split('\t');
        let (Some(_stamp), Some(heard), Some(old)) = (f.next(), f.next(), f.next()) else {
            continue;
        };
        let wav = f.next().filter(|w| !w.is_empty());
        let mut rescored = None;
        let heard_now = match (wav, asr.as_deref_mut()) {
            (Some(w), Some(asr)) => {
                match read_wav(Path::new(w)).and_then(|a| asr.transcribe(&a).ok()) {
                    Some(t) => {
                        // Same order as live: real intents, the audio, catch-alls.
                        if reader.read_one(&t, &ctx).strict.is_none() {
                            rescored = rescore::run(asr, "", &t, reader.sound_alikes(), |cs| {
                                reader
                                    .read(cs, &ctx)
                                    .into_iter()
                                    .map(|p| p.strict)
                                    .collect()
                            });
                        }
                        t
                    }
                    None => heard.to_owned(),
                }
            }
            _ => heard.to_owned(),
        };
        if let Some(r) = &rescored {
            println!("        {r}");
        }
        let read = reader.read_one(&heard_now, &ctx);
        let new = read
            .strict
            .or_else(|| rescored.filter(|r| r.accepted).map(|r| r.action))
            .map(|a| kind_of(&a))
            .or_else(|| reader.classify(classifier.as_deref_mut()?, &heard_now, &ctx))
            .or_else(|| read.fallback.map(|a| kind_of(&a)));
        let outcome = compare(old, new.as_deref());
        let label = match outcome {
            Outcome::Same => {
                same += 1;
                continue;
            }
            Outcome::Fixed => {
                fixed += 1;
                "FIXED  "
            }
            Outcome::Broken => {
                broken += 1;
                "BROKEN "
            }
            Outcome::Changed => {
                changed += 1;
                "CHANGED"
            }
        };
        let retranscribed = if heard_now == heard {
            String::new()
        } else {
            format!(" (now heard {heard_now:?})")
        };
        println!(
            "{label} {heard:?}{retranscribed}: {} -> {}",
            if old.is_empty() { "-" } else { old },
            new.as_deref().unwrap_or("-")
        );
    }
    println!("{same} same, {fixed} fixed, {changed} changed, {broken} broken");
    i32::from(broken > 0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::skills::Action;
    use crate::skills::fun::Fun;
    use crate::skills::info::Info;
    use crate::skills::kagi::Query;
    use crate::skills::media::Media;
    use crate::store;
    use std::env;
    use std::fs;
    use std::process;

    #[test]
    fn golden_lines_use_a_fixed_clock() {
        assert_eq!(golden_line("tell a joke"), "tell a joke\tFun(Joke)\t-");
        assert_eq!(golden_line("tell a joke"), golden_line("tell a joke"));
        // Wednesday, so Friday is two days ahead.
        let line = golden_line("what's on my calendar on friday");
        assert!(line.contains("day: 2,"), "{line}");
        assert_eq!(golden_line("blorp"), "blorp\t-\t-");
    }

    #[test]
    fn kinds() {
        assert_eq!(kind(&Action::Fun(Fun::Joke)), "Fun(Joke)");
        assert_eq!(kind(&Action::Media(Media::Radio(3))), "Media(Radio");
        assert_eq!(
            kind(&Action::Info(Info::Weather {
                day: 1,
                place: None,
                part: None
            })),
            "Info(Weather"
        );
        assert_eq!(kind(&Action::Kagi(Query("who".into()))), "Kagi(Query");
        assert_eq!(kind(&Action::Repeat), "Repeat");
    }

    #[test]
    fn rust_reader_reads_both_tiers() {
        let ctx = Context::at(9 * 3600);
        let got = Reader::Rust.read(&["tell a joke".into(), "blorp".into()], &ctx);
        assert_eq!(got[0].strict.as_deref(), Some("Fun(Joke)"));
        assert_eq!(got[1], Parses::default());
        assert_eq!(kind_of("Media(Radio(3))"), "Media(Radio");
    }

    #[test]
    fn outcomes() {
        assert_eq!(compare("", Some("Fun(Joke")), Outcome::Fixed);
        assert_eq!(compare("Fun(Joke", None), Outcome::Broken);
        assert_eq!(compare("Fun(Joke", Some("Fun(Joke")), Outcome::Same);
        assert_eq!(compare("Fun(Joke", Some("Chat(Hello")), Outcome::Changed);
        assert_eq!(compare("", None), Outcome::Same);
    }

    #[test]
    fn wav_roundtrip() {
        let dir = env::temp_dir().join(format!("va-replay-{}", process::id()));
        let path = dir.join("x.wav");
        store::write_wav(&path, &[0, 16384, -16384]).expect("writes");
        assert_eq!(read_wav(&path), Some(vec![0.0, 0.5, -0.5]));
        let _ = fs::remove_dir_all(dir);
    }
}
