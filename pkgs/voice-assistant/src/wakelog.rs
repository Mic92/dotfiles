//! Wake word training material: the audio before every trigger and every
//! near miss, with what happened next as a rough label.
//!
//! `$XDG_STATE_HOME/voice-assistant/wakewords.tsv` columns:
//! milliseconds, kind (trigger|near), probability, outcome, transcript, wav.
//! A trigger followed by a command is almost surely the user; one followed
//! by silence is a likely false positive; near misses need a listen.

use crate::store;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

/// Peaks above this are kept even though they did not trigger.
const NEAR: f32 = 0.15;
/// The peak is over once it has not risen for this long.
const SETTLE: Duration = Duration::from_millis(500);
/// A TV saying "Alexandra" all evening must not fill the disk.
const NEAR_EVERY: Duration = Duration::from_secs(10);

struct Pending {
    stamp: u128,
    p: f32,
    wav: PathBuf,
}

pub struct WakeLog {
    dir: Option<PathBuf>,
    threshold: f32,
    /// A trigger waiting for its outcome.
    pending: Option<Pending>,
    /// Rising near miss: highest probability and when it was reached.
    near: Option<(f32, Instant)>,
    last_near: Option<Instant>,
}

impl WakeLog {
    /// `dir` is the state directory; `None` disables logging.
    pub fn new(dir: Option<PathBuf>, threshold: f32) -> Self {
        WakeLog {
            dir,
            threshold,
            pending: None,
            near: None,
            last_near: None,
        }
    }

    fn save(&self, kind: &str, p: f32, audio: &[i16]) -> Option<(u128, PathBuf)> {
        let dir = self.dir.as_ref()?;
        let stamp = store::unix_ms();
        let wav = dir
            .join("wakewords")
            .join(format!("{stamp}-{kind}-{p:.2}.wav"));
        match store::write_wav(&wav, audio) {
            Ok(()) => Some((stamp, wav)),
            Err(e) => {
                eprintln!("voice-assistant: {}: {e}", wav.display());
                None
            }
        }
    }

    fn row(&self, stamp: u128, kind: &str, p: f32, outcome: &str, heard: &str, wav: &Path) {
        let Some(dir) = &self.dir else {
            return;
        };
        let line = store::tsv(&[
            &stamp.to_string(),
            kind,
            &format!("{p:.3}"),
            outcome,
            heard,
            &wav.display().to_string(),
        ]);
        if let Err(e) = store::append(&dir.join("wakewords.tsv"), &line) {
            eprintln!("voice-assistant: logging wake word: {e}");
        }
    }

    /// The wake word fired on `audio` (the seconds before it).
    pub fn triggered(&mut self, p: f32, audio: &[i16]) {
        self.near = None;
        self.pending = self
            .save("trigger", p, audio)
            .map(|(stamp, wav)| Pending { stamp, p, wav });
    }

    /// How the conversation a trigger started went: "command", "miss",
    /// "no-speech", ... A no-op for follow-ups heard without a wake word.
    pub fn outcome(&mut self, outcome: &str, heard: &str) {
        if let Some(t) = self.pending.take() {
            self.row(t.stamp, "trigger", t.p, outcome, heard, &t.wav);
        }
    }

    /// Called while idle with the detector's peak since the last call;
    /// `audio` provides the recent seconds once a near miss has settled.
    pub fn observe(&mut self, peak: f32, audio: impl FnOnce() -> Vec<i16>) {
        let now = Instant::now();
        if (NEAR..self.threshold).contains(&peak) && self.near.is_none_or(|(best, _)| peak > best) {
            if self.near.is_none()
                && self
                    .last_near
                    .is_some_and(|t| now.duration_since(t) < NEAR_EVERY)
            {
                return;
            }
            self.near = Some((peak, now));
            return;
        }
        let Some((best, at)) = self.near else {
            return;
        };
        if now.duration_since(at) < SETTLE {
            return;
        }
        self.near = None;
        self.last_near = Some(now);
        if let Some((stamp, wav)) = self.save("near", best, &audio()) {
            self.row(stamp, "near", best, "near", "", &wav);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::fs;
    use std::path::Path;
    use std::process;

    /// Per test, as tests run in parallel within the same millisecond.
    fn dir(test: &str) -> PathBuf {
        let d = env::temp_dir().join(format!("va-wakelog-{}-{test}", process::id()));
        let _ = fs::create_dir_all(&d);
        d
    }

    fn rows(d: &Path) -> Vec<Vec<String>> {
        fs::read_to_string(d.join("wakewords.tsv"))
            .unwrap_or_default()
            .lines()
            .map(|l| l.split('\t').map(str::to_owned).collect())
            .collect()
    }

    #[test]
    fn trigger_gets_its_outcome() {
        let d = dir("trigger");
        let mut log = WakeLog::new(Some(d.clone()), 0.4);
        log.outcome("command", "ignored, nothing pending");
        log.triggered(0.73, &[0; 160]);
        log.outcome("command", "What time is it?");
        log.outcome("miss", "follow-up without wake word");
        let r = rows(&d);
        assert_eq!(r.len(), 1);
        assert_eq!(
            &r[0][1..5],
            ["trigger", "0.730", "command", "What time is it?"]
        );
        assert!(Path::new(&r[0][5]).exists());
        let _ = fs::remove_dir_all(d);
    }

    #[test]
    fn near_miss_is_saved_once_settled() {
        let d = dir("near");
        let mut log = WakeLog::new(Some(d.clone()), 0.4);
        log.observe(0.05, Vec::new);
        log.observe(0.2, Vec::new);
        log.observe(0.3, Vec::new);
        // Not yet settled.
        log.observe(0.0, || vec![0; 160]);
        assert!(rows(&d).is_empty());
        log.near = log.near.map(|(p, t)| (p, t - SETTLE));
        log.observe(0.0, || vec![0; 160]);
        let r = rows(&d);
        assert_eq!(r.len(), 1);
        assert_eq!(&r[0][1..4], ["near", "0.300", "near"]);
        // Rate limited.
        log.observe(0.3, Vec::new);
        assert!(log.near.is_none());
        let _ = fs::remove_dir_all(d);
    }

    #[test]
    fn disabled_without_dir() {
        let mut log = WakeLog::new(None, 0.4);
        log.triggered(0.9, &[0; 10]);
        log.outcome("command", "x");
    }
}
