//! Second opinion for transcripts nothing understood: score similar
//! sounding commands against the same audio and take one if the
//! recogniser finds it about as likely as what it wrote.

use std::fmt;
use std::time::Instant;

use crate::asr::Asr;
use crate::phonetic;

/// Sound-alike commands to score against the audio.
const CANDIDATES: usize = 8;
/// How much less likely (log-probability) than the recogniser's own
/// transcript a command may be and still be taken. On recorded misses the
/// intended command scored 0-9 below the transcript ("Same for me" ->
/// "Sing for me" at -9), the best wrong one 18 or more.
const MARGIN: f32 = 10.0;

pub struct Rescored<A> {
    pub heard: String,
    pub heard_score: f32,
    pub best: String,
    pub score: f32,
    pub action: A,
    pub accepted: bool,
    candidates: usize,
    secs: f32,
}

impl<A> fmt::Display for Rescored<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "rescored {:?} ({:.1}) -> {:?} ({:.1}), {} of {} candidates in {:.2}s",
            self.heard,
            self.heard_score,
            self.best,
            self.score,
            if self.accepted {
                "took best"
            } else {
                "rejected all"
            },
            self.candidates,
            self.secs
        )
    }
}

/// Names spelled as the recogniser writes them ("Ping Eve", "Play Soma
/// Fm"). Grammar candidates come lowercase ("Ping eve.").
fn capitalised(text: &str) -> String {
    text.split(' ')
        .map(|w| {
            let mut c = w.chars();
            c.next()
                .map(|f| f.to_uppercase().chain(c).collect())
                .unwrap_or_default()
        })
        .collect::<Vec<String>>()
        .join(" ")
}

/// `asr` must still hold the audio `heard` was transcribed from, with
/// `said_before` (the wake word) in front of it. `parse` reads all
/// candidates at once, as the Lisp brain takes one round trip for them.
pub fn run<A>(
    asr: &mut Asr,
    said_before: &str,
    heard: &str,
    alikes: &SoundAlikes,
    parse: impl FnOnce(&[String]) -> Vec<Option<A>>,
) -> Option<Rescored<A>> {
    let mut candidates = vec![heard.to_owned()];
    candidates.extend(alikes.find(heard, CANDIDATES));
    if candidates.len() < 2 {
        return None;
    }
    let start = Instant::now();
    // Each candidate as written and capitalised. The likelier one counts.
    let spoken: Vec<String> = candidates
        .iter()
        .flat_map(|c| {
            let c = format!("{said_before}{c}");
            [capitalised(&c), c]
        })
        .collect();
    let scores: Vec<f32> = asr
        .score(&spoken)
        .inspect_err(|e| eprintln!("voice-assistant: rescoring: {e}"))
        .ok()?
        .chunks(2)
        .map(|p| p.iter().copied().fold(f32::NEG_INFINITY, f32::max))
        .collect();
    let heard_score = scores.first().copied()?;
    let (best, score, action) = candidates[1..]
        .iter()
        .zip(&scores[1..])
        .zip(parse(&candidates[1..]))
        .filter_map(|((c, s), a)| Some((c, *s, a?)))
        .max_by(|a, b| a.1.total_cmp(&b.1))?;
    Some(Rescored {
        heard: heard.to_owned(),
        heard_score,
        best: best.clone(),
        score,
        action,
        accepted: score >= heard_score - MARGIN,
        candidates: candidates.len() - 1,
        secs: start.elapsed().as_secs_f32(),
    })
}

pub struct SoundAlikes {
    sentences: Vec<String>,
    lexicon: phonetic::Lexicon,
}

/// How far a transcript may sound from a command to still be offered.
/// On recorded and synthetic misses this kept 77 of 82 intended commands
/// and 3 of 80 unrelated sentences.
const SOUND_ALIKE: f32 = 0.4;

impl SoundAlikes {
    /// Phonemizing every command takes a quarter second.
    pub fn new(sentences: Vec<String>) -> Self {
        let lexicon = phonetic::Lexicon::new(sentences.iter().map(|s| phonetic::phonemes(s)));
        SoundAlikes { sentences, lexicon }
    }

    /// Commands that sound like `heard`, closest first, as the recogniser
    /// would write them: candidates for rescoring a transcript nothing
    /// understood.
    pub fn find(&self, heard: &str, limit: usize) -> Vec<String> {
        let h = phonetic::Heard::new(heard);
        if h.len() < 4 {
            return Vec::new();
        }
        self.lexicon
            .nearest(&h, SOUND_ALIKE, limit)
            .into_iter()
            .take(limit)
            .map(|(_, i)| {
                let mut c = self.sentences[i].chars();
                c.next()
                    .map(|f| format!("{}{}.", f.to_uppercase(), c.as_str()))
                    .unwrap_or_default()
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brain::{Brain, Shared};
    use crate::clock::Clock;
    use std::sync::LazyLock;

    /// The brain's commands, or `None` without one to start.
    static BRAIN: LazyLock<Option<Shared>> = LazyLock::new(|| {
        Brain::connect()
            .inspect_err(|e| eprintln!("no brain, skipping: {e}"))
            .ok()
            .map(Shared::new)
    });

    fn brain() -> Option<&'static Shared> {
        BRAIN.as_ref()
    }

    fn named() -> &'static SoundAlikes {
        static EMPTY: LazyLock<SoundAlikes> = LazyLock::new(|| SoundAlikes::new(Vec::new()));
        brain().map_or(&EMPTY, Shared::sound_alikes)
    }

    #[test]
    fn capitalises_every_word() {
        assert_eq!(capitalised("Ping eve."), "Ping Eve.");
        assert_eq!(capitalised("play soma fm."), "Play Soma Fm.");
        assert_eq!(capitalised(""), "");
    }

    #[test]
    fn sound_alikes_from_any_sentences() {
        let s = SoundAlikes::new(vec!["tell a joke".into(), "ping eve".into()]);
        assert_eq!(s.find("Tell a choke", 8), ["Tell a joke."]);
        assert!(s.find("ok", 8).is_empty());
    }

    #[test]
    fn sound_alikes_for_mangled_names() {
        if brain().is_none() {
            return;
        }
        let c = named().find("PingYiF.", 8);
        assert!(c.iter().any(|s| s == "Ping eve."), "{c:?}");
        assert!(c.iter().any(|s| s == "Ping eva."), "{c:?}");
        let c = named().find("Play Laslos Radio.", 5);
        assert!(c.iter().any(|s| s.contains("lassulus")), "{c:?}");
        assert!(named().find("ok", 5).is_empty());
        // "tell a joke", heard through a headset
        for heard in ["Tell it shalk", "tell it chalk."] {
            let c = named().find(heard, 8);
            assert!(c.iter().any(|s| s.ends_with(" joke.")), "{heard}: {c:?}");
        }
    }

    /// `cargo test --release bench_sound_alikes -- --ignored --nocapture`
    #[test]
    #[ignore]
    fn bench_sound_alikes() {
        if brain().is_none() {
            return;
        }
        let SoundAlikes { sentences, lexicon } = named();
        let phones: Vec<Vec<phonetic::Phone>> =
            sentences.iter().map(|s| phonetic::phonemes(s)).collect();
        let heard = phonetic::Heard::new("tell it chalk");
        let n = 100;
        let start = std::time::Instant::now();
        for _ in 0..n {
            std::hint::black_box(lexicon.nearest(&heard, SOUND_ALIKE, 8));
        }
        eprintln!("trie: {:?} per lookup", start.elapsed() / n);
        let start = std::time::Instant::now();
        for _ in 0..n {
            std::hint::black_box(
                phones
                    .iter()
                    .filter_map(|p| heard.distance(p, SOUND_ALIKE))
                    .count(),
            );
        }
        eprintln!("pairwise: {:?} per lookup", start.elapsed() / n);
    }

    #[test]
    fn trie_agrees_with_pairwise() {
        if brain().is_none() {
            return;
        }
        let SoundAlikes { sentences, lexicon } = named();
        for text in ["tell it chalk", "Peng Yif", "switch on the y", "hum dum"] {
            let heard = phonetic::Heard::new(text);
            let mut all: Vec<(f32, usize)> = sentences
                .iter()
                .enumerate()
                .filter_map(|(i, s)| Some((heard.distance(&phonetic::phonemes(s), 0.4)?, i)))
                .collect();
            all.sort_by(|a, b| a.0.total_cmp(&b.0));
            let trie: Vec<f32> = lexicon
                .nearest(&heard, 0.4, 8)
                .iter()
                .map(|x| x.0)
                .collect();
            let pairs: Vec<f32> = all.iter().take(8).map(|x| x.0).collect();
            assert_eq!(trie, pairs, "{text}");
        }
    }

    #[test]
    fn sound_alikes_find_intended_commands() {
        let Some(b) = brain() else {
            return;
        };
        let cases: Vec<(&str, &str)> = include_str!("sound_alikes.tsv")
            .lines()
            .filter(|l| !l.starts_with('#'))
            .filter_map(|l| l.split_once('\t'))
            .collect();
        let start = std::time::Instant::now();
        named().find("warm up", 1);
        let built = start.elapsed();
        let start = std::time::Instant::now();
        let found = cases
            .iter()
            .filter(|(heard, want)| {
                let c = named().find(heard, 8);
                let ok = b
                    .parse(&c, &Clock::at(0))
                    .iter()
                    .any(|a| a.as_deref() == Some(*want));
                if !ok {
                    eprintln!("miss {heard} ({want}): {c:?}");
                }
                ok
            })
            .count();
        eprintln!(
            "{found} of {}, table in {built:?}, lookups in {:?}",
            cases.len(),
            start.elapsed()
        );
        assert!(found >= 71, "{found} of {}", cases.len());
    }
}
