//! Second opinion for transcripts nothing understood: score similar
//! sounding commands against the same audio and take one if the
//! recogniser finds it about as likely as what it wrote.

use std::fmt;
use std::time::Instant;

use crate::asr::Asr;
use crate::skills::SoundAlikes;

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn capitalises_every_word() {
        assert_eq!(capitalised("Ping eve."), "Ping Eve.");
        assert_eq!(capitalised("play soma fm."), "Play Soma Fm.");
        assert_eq!(capitalised(""), "");
    }
}
