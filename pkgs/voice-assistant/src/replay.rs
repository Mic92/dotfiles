//! `--replay`: re-run everything logged in heard.tsv through the current
//! grammar (and speech recogniser, where audio was kept) and report what
//! changed.

use crate::asr::Asr;
use crate::rescore;
use crate::skills::{self, Context};
use std::fs;
use std::path::Path;

/// "Info(Weather", "Fun(Joke)": the action without its slot values, which
/// may legitimately differ between runs (timestamps, random picks).
pub fn kind(a: &skills::Action) -> String {
    let d = format!("{a:?}");
    let mut end = d.len();
    for (n, (i, _)) in d.match_indices(['(', ' ', '{', '"']).enumerate() {
        if n == 1 || d[i..].starts_with([' ', '{', '"']) {
            end = i;
            break;
        }
    }
    d[..end].to_owned()
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
pub fn run(log: &Path, mut asr: Option<&mut Asr>) -> i32 {
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
                        if skills::parse_tier(&t, &ctx, false).is_none() {
                            rescored = rescore::run(asr, "", &t, &ctx);
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
        let new = skills::parse_tier(&heard_now, &ctx, false)
            .or_else(|| rescored.filter(|r| r.accepted).map(|r| r.action))
            .or_else(|| skills::parse_tier(&heard_now, &ctx, true))
            .map(|a| kind(&a));
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
