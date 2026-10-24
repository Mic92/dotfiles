//! Second opinion for transcripts nothing understood: score similar
//! sounding commands against the same audio and take one if whisper finds
//! it about as likely as what it wrote.

use std::fmt;
use std::time::Instant;

use crate::skills::{self, Action, Context};
use crate::whisper::Whisper;

/// Sound-alike commands to score against the audio.
const CANDIDATES: usize = 8;
/// How much less likely (log-probability) than whisper's own transcript a
/// command may be and still be taken: whisper's best guess at an unknown
/// name is barely likelier than the name it should have written.
const MARGIN: f32 = 2.0;

pub struct Rescored {
    pub heard: String,
    pub heard_score: f32,
    pub best: String,
    pub score: f32,
    pub action: Action,
    pub accepted: bool,
    candidates: usize,
    secs: f32,
}

impl fmt::Display for Rescored {
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

/// `whisper` must still hold the audio `heard` was transcribed from, with
/// `said_before` (the wake word) in front of it.
pub fn run(
    whisper: &mut Whisper,
    said_before: &str,
    heard: &str,
    ctx: &Context,
) -> Option<Rescored> {
    let mut candidates = vec![heard.to_owned()];
    candidates.extend(skills::sound_alikes(heard, CANDIDATES));
    if candidates.len() < 2 {
        return None;
    }
    let start = Instant::now();
    let spoken: Vec<String> = candidates
        .iter()
        .map(|c| format!("{said_before}{c}"))
        .collect();
    let scores = whisper
        .score(&spoken)
        .inspect_err(|e| eprintln!("voice-assistant: rescoring: {e}"))
        .ok()?;
    let heard_score = scores.first().copied()?;
    let (best, score, action) = candidates
        .iter()
        .zip(&scores)
        .skip(1)
        .filter_map(|(c, s)| Some((c, *s, skills::parse_tier(c, ctx, false)?)))
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
