//! `--replay`: re-run everything logged in heard.tsv through the current
//! brain (and speech recogniser, where audio was kept) and report what
//! changed.

use crate::asr::Asr;
use crate::brain::{Parses, Shared};
use crate::classify::Classifier;
use crate::clock::Clock;
use crate::rescore;
use std::fs;
use std::path::Path;

/// "Info(Weather", "Fun(Joke)": an action as the brain shows it, without
/// its slot values, which may legitimately differ between runs
/// (timestamps, random picks).
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

/// The kind of action the classifier stands for, if sure.
fn classify(b: &Shared, c: &mut Classifier, text: &str, ctx: &Clock) -> Option<String> {
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

fn read_one(b: &Shared, text: &str, ctx: &Clock) -> Parses {
    b.parses(&[text.to_owned()], ctx).pop().unwrap_or_default()
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
    brain: &Shared,
    mut asr: Option<&mut Asr>,
    mut classifier: Option<&mut Classifier>,
) -> i32 {
    let Ok(text) = fs::read_to_string(log) else {
        eprintln!("voice-assistant: nothing to replay in {}", log.display());
        return 1;
    };
    let ctx = Clock::local();
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
                        if read_one(brain, &t, &ctx).strict.is_none() {
                            rescored = rescore::run(asr, "", &t, brain.sound_alikes(), |cs| {
                                brain.parse(cs, &ctx)
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
        let read = read_one(brain, &heard_now, &ctx);
        let new = read
            .strict
            .or_else(|| rescored.filter(|r| r.accepted).map(|r| r.action))
            .map(|a| kind_of(&a))
            .or_else(|| classify(brain, classifier.as_deref_mut()?, &heard_now, &ctx))
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
    use crate::store;
    use std::env;
    use std::fs;
    use std::process;

    #[test]
    fn kinds() {
        assert_eq!(kind_of("Fun(Joke)"), "Fun(Joke)");
        assert_eq!(kind_of("Media(Radio(3))"), "Media(Radio");
        assert_eq!(
            kind_of("Info(Weather { day: 1, place: None, part: None })"),
            "Info(Weather"
        );
        assert_eq!(kind_of("Kagi(Query(\"who\"))"), "Kagi(Query");
        assert_eq!(kind_of("Repeat"), "Repeat");
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
