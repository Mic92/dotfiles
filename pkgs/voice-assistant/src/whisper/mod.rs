//! whisper.cpp: Silero VAD for end-of-utterance, speech-to-text, and
//! scoring candidate texts against the audio.

mod ffi;

use std::ffi::CString;
use std::path::PathBuf;

use ffi::{Context, FullParams, Token, c_int};
pub use ffi::{Vad, init};

const RATE: usize = 16000;
const NO_SPEECH_PROB: f32 = 0.6;
/// Below ~384 frames whisper loops on short words ("reboot reboot reboot").
const MIN_AUDIO_CTX: usize = 512;

/// A word and how sure whisper was of its least certain token.
#[derive(Debug, Clone, PartialEq)]
pub struct Word {
    pub text: String,
    pub p: f32,
}

#[derive(Debug, Default, Clone)]
pub struct Transcript {
    pub text: String,
    pub words: Vec<Word>,
}

impl Transcript {
    /// Tokens starting with a space begin a new word.
    fn push_token(&mut self, token: &str, p: f32) {
        self.text.push_str(token);
        match self.words.last_mut() {
            Some(w) if !token.starts_with(' ') => {
                w.text.push_str(token);
                w.p = w.p.min(p);
            }
            _ => self.words.push(Word {
                text: token.trim_start().to_owned(),
                p,
            }),
        }
    }
}

fn log_softmax(logits: &[f32]) -> Vec<f32> {
    let max = logits.iter().copied().fold(f32::NEG_INFINITY, f32::max);
    let sum: f32 = logits.iter().map(|l| (l - max).exp()).sum();
    let norm = max + sum.ln();
    logits.iter().map(|l| l - norm).collect()
}

/// Speech-to-text model, loaded on the first command and then kept in RAM.
pub struct Whisper {
    model: PathBuf,
    language: CString,
    prompt: CString,
    /// Prompt of the last transcription, which scoring must condition on too.
    last_prompt: CString,
    threads: i32,
    ctx: Option<Context>,
}

impl Whisper {
    pub fn new(model: PathBuf, language: &str, prompt: &str, threads: i32) -> Result<Self, String> {
        Ok(Whisper {
            model,
            language: CString::new(language).map_err(|e| e.to_string())?,
            prompt: CString::new(prompt).map_err(|e| e.to_string())?,
            last_prompt: CString::default(),
            threads: threads.max(1),
            ctx: None,
        })
    }

    fn ctx(&mut self) -> Result<&mut Context, String> {
        if self.ctx.is_none() {
            self.ctx = Some(Context::load(&self.model)?);
        }
        self.ctx
            .as_mut()
            .ok_or_else(|| "whisper context missing".into())
    }

    /// `prompt` replaces the default one, e.g. with the answers to a question.
    pub fn transcribe(
        &mut self,
        samples: &[f32],
        prompt: Option<&str>,
    ) -> Result<Transcript, String> {
        self.last_prompt = match prompt {
            Some(p) => CString::new(p).map_err(|e| e.to_string())?,
            None => self.prompt.clone(),
        };
        // The encoder covers 30 s (1500 frames) unless told otherwise; commands
        // are a few seconds, so size it to the audio plus margin.
        let audio_ctx = c_int(((samples.len() / RATE + 1) * 50 + 64).clamp(MIN_AUDIO_CTX, 1500))?;
        let (threads, language, prompt) = (
            self.threads,
            self.language.clone(),
            self.last_prompt.clone(),
        );
        let ctx = self.ctx()?;
        ctx.full(
            &FullParams {
                threads,
                language: &language,
                prompt: &prompt,
                audio_ctx,
            },
            samples,
        )?;
        let eot = ctx.special().eot;
        let mut out = Transcript::default();
        // With an initial prompt, whisper turns noise into a copy of the
        // prompt; its own no-speech estimate catches that.
        for seg in ctx
            .segments()
            .into_iter()
            .filter(|s| s.no_speech <= NO_SPEECH_PROB)
        {
            for t in seg.tokens.into_iter().filter(|t| t.id < eot) {
                out.push_token(&t.text, t.p);
            }
        }
        out.text = out.text.trim().to_owned();
        Ok(out)
    }

    /// Log-probability of each candidate text given the audio of the last
    /// `transcribe`, conditioned on the same prompt. Candidates sharing a
    /// prefix share its decoder passes.
    pub fn score(&mut self, candidates: &[String]) -> Result<Vec<f32>, String> {
        let (threads, prompt, language) = (
            self.threads,
            self.last_prompt.clone(),
            self.language.clone(),
        );
        let ctx = self.ctx()?;
        let special = ctx.special();
        let mut prefix = vec![special.prev];
        let mut p = ctx.tokenize(&format!(" {}", prompt.to_string_lossy().trim()))?;
        // whisper_full keeps only the prompt's tail.
        p.drain(..p.len().saturating_sub(ctx.max_prompt()));
        prefix.extend(p);
        prefix.push(special.sot);
        prefix.extend(ctx.language_token(&language));
        prefix.extend([special.transcribe, special.no_timestamps]);
        let seqs = candidates
            .iter()
            .map(|c| {
                let mut t = ctx.tokenize(&format!(" {}", c.trim()))?;
                t.push(special.eot);
                Ok(t)
            })
            .collect::<Result<Vec<Vec<Token>>, String>>()?;
        let first = log_softmax(ctx.decode(&prefix, 0, threads)?);
        let base = prefix.len();
        trie_scores(&seqs, first, |tok, depth| {
            ctx.decode(&[tok], base + depth, threads).map(log_softmax)
        })
    }
}

/// Score token sequences given the log-probs after the shared prefix and a
/// step function `(token, depth) -> log-probs after it`, visiting
/// sequences in sorted order so common prefixes are stepped through once.
fn trie_scores(
    seqs: &[Vec<Token>],
    first: Vec<f32>,
    mut step: impl FnMut(Token, usize) -> Result<Vec<f32>, String>,
) -> Result<Vec<f32>, String> {
    let mut order: Vec<usize> = (0..seqs.len()).collect();
    order.sort_by(|&a, &b| seqs[a].cmp(&seqs[b]));
    // rows[d]: log-probs after `path[..d]`; cum[d]: score of `path[..d]`.
    let mut rows = vec![first];
    let mut cum = vec![0.0f32];
    let mut path: Vec<Token> = Vec::new();
    let mut scores = vec![f32::NEG_INFINITY; seqs.len()];
    for i in order {
        let seq = &seqs[i];
        let common = path.iter().zip(seq).take_while(|(a, b)| a == b).count();
        path.truncate(common);
        rows.truncate(common + 1);
        cum.truncate(common + 1);
        for (d, &tok) in seq.iter().enumerate().skip(common) {
            let lp = usize::try_from(tok)
                .ok()
                .and_then(|t| rows[d].get(t))
                .copied()
                .unwrap_or(f32::NEG_INFINITY);
            cum.push(cum[d] + lp);
            path.push(tok);
            if d + 1 < seq.len() {
                rows.push(step(tok, d)?);
            }
        }
        scores[i] = cum[seq.len()];
    }
    Ok(scores)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn words_from_tokens() {
        let mut t = Transcript::default();
        for (tok, p) in [
            (" Play", 0.9),
            (" Las", 0.4),
            ("los", 0.7),
            (" radio", 0.95),
            (".", 0.99),
        ] {
            t.push_token(tok, p);
        }
        assert_eq!(t.text, " Play Laslos radio.");
        let w: Vec<(&str, f32)> = t.words.iter().map(|w| (w.text.as_str(), w.p)).collect();
        assert_eq!(w, [("Play", 0.9), ("Laslos", 0.4), ("radio.", 0.95)]);
    }

    #[test]
    fn log_softmax_normalises() {
        let l = log_softmax(&[1.0, 2.0, 3.0]);
        let total: f32 = l.iter().map(|x| x.exp()).sum();
        assert!((total - 1.0).abs() < 1e-5);
        assert!(l[2] > l[1] && l[1] > l[0]);
    }

    #[test]
    fn trie_shares_prefixes() {
        // Token t always has log-prob -t, so a score is minus the token sum;
        // count steps to see shared prefixes walked once.
        let row = |_: usize| (0..10).map(|t| -(t as f32)).collect::<Vec<f32>>();
        let mut steps = 0;
        let seqs = vec![vec![1, 2, 9], vec![1, 3, 9], vec![1, 2, 9], vec![4, 9]];
        let scores = trie_scores(&seqs, row(0), |_, d| {
            steps += 1;
            Ok(row(d))
        })
        .unwrap_or_default();
        assert_eq!(scores, [-12.0, -13.0, -12.0, -13.0]);
        // 1, 1→2, 1→3, 4: the duplicate costs nothing.
        assert_eq!(steps, 4);
    }
}
