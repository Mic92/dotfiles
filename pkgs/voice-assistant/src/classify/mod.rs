//! Nearest example sentence by gte-small embedding, for sentences the
//! grammar misses.

mod wordpiece;

use std::collections::HashSet;
use std::collections::hash_map::DefaultHasher;
use std::env;
use std::fmt;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use ort::session::Session;

use crate::onnx;
use wordpiece::WordPiece;

const BATCH: usize = 64;
const CLINC: &str = include_str!("clinc.tsv");

struct Embedder {
    session: Session,
    vocab: WordPiece,
    token_types: bool,
}

impl Embedder {
    fn load(dir: &Path) -> Result<Self, String> {
        let vocab_path = dir.join("vocab.txt");
        let vocab = WordPiece::parse(
            &fs::read_to_string(&vocab_path)
                .map_err(|e| format!("{}: {e}", vocab_path.display()))?,
        )?;
        let session = onnx::session(&dir.join("model.onnx"), 1)?;
        let token_types = session
            .inputs()
            .iter()
            .any(|i| i.name() == "token_type_ids");
        Ok(Embedder {
            session,
            vocab,
            token_types,
        })
    }

    fn embed(&mut self, texts: &[&str]) -> Result<Vec<f32>, String> {
        let mut out = Vec::new();
        for chunk in texts.chunks(BATCH) {
            let ids: Vec<Vec<i64>> = chunk.iter().map(|t| self.vocab.encode(t)).collect();
            let n = ids.iter().map(Vec::len).max().unwrap_or(0);
            let b = chunk.len();
            let mut flat = vec![0i64; b * n];
            let mut mask = vec![0i64; b * n];
            for (i, row) in ids.iter().enumerate() {
                flat[i * n..i * n + row.len()].copy_from_slice(row);
                mask[i * n..i * n + row.len()].fill(1);
            }
            let mut inputs = ort::inputs![
                "input_ids" => onnx::tensor(&[b, n], flat)?,
                "attention_mask" => onnx::tensor(&[b, n], mask.clone())?,
            ];
            if self.token_types {
                inputs.push((
                    "token_type_ids".into(),
                    onnx::tensor(&[b, n], vec![0i64; b * n])?.into(),
                ));
            }
            let res = self
                .session
                .run(inputs)
                .map_err(|e| format!("embedder: {e}"))?;
            let (shape, hidden) = onnx::output(&res, "last_hidden_state")?;
            let [_, _, d] = shape[..] else {
                return Err(format!("embedder output shape {shape:?}"));
            };
            for (i, row) in ids.iter().enumerate() {
                let mut v = vec![0f32; d];
                for t in 0..row.len() {
                    let h = &hidden[(i * n + t) * d..][..d];
                    v.iter_mut().zip(h).for_each(|(a, x)| *a += x);
                }
                let norm = v.iter().map(|x| x * x).sum::<f32>().sqrt().max(1e-9);
                out.extend(v.iter().map(|x| x / norm));
            }
        }
        Ok(out)
    }
}

/// The example nearest to a sentence, and how clearly.
pub struct Nearest {
    /// What the nearest example asks for, e.g. "Timer(Start".
    pub intent: String,
    pub score: f32,
    pub lead: f32,
    pub example: String,
}

impl fmt::Display for Nearest {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "nearest {} ({:.3}, lead {:.3}, like {:?})",
            self.intent, self.score, self.lead, self.example
        )
    }
}

pub struct Classifier {
    embedder: Embedder,
    texts: Vec<String>,
    /// Index into `kinds` per example.
    labels: Vec<usize>,
    kinds: Vec<String>,
    vectors: Vec<f32>,
    dim: usize,
}

fn cache_path(model: &Path, texts: &[String]) -> Option<PathBuf> {
    let base = env::var_os("XDG_CACHE_HOME")
        .map(PathBuf::from)
        .or_else(|| env::var_os("HOME").map(|h| PathBuf::from(h).join(".cache")))?;
    let mut h = DefaultHasher::new();
    model.hash(&mut h);
    texts.hash(&mut h);
    Some(
        base.join("voice-assistant")
            .join(format!("intents-{:016x}.bin", h.finish())),
    )
}

fn read_vectors(path: &Path) -> Option<Vec<f32>> {
    let bytes = fs::read(path).ok()?;
    let (chunks, rest) = bytes.as_chunks::<4>();
    rest.is_empty()
        .then(|| chunks.iter().map(|b| f32::from_le_bytes(*b)).collect())
}

impl Classifier {
    /// Learns from (text, kind) examples, as the brain lists them.
    pub fn new(dir: &Path, mut examples: Vec<(String, String)>) -> Result<Self, String> {
        let mut embedder = Embedder::load(dir)?;
        let known: HashSet<String> = examples.iter().map(|(_, k)| k.clone()).collect();
        examples.extend(
            CLINC
                .lines()
                .filter_map(|l| l.split_once('\t'))
                .filter(|(k, _)| known.contains(*k))
                .map(|(k, t)| (t.to_owned(), k.to_owned())),
        );
        let mut kinds: Vec<String> = known.into_iter().collect();
        kinds.sort();
        let (texts, labels): (Vec<String>, Vec<usize>) = examples
            .into_iter()
            .map(|(t, k)| (t, kinds.binary_search(&k).unwrap_or(0)))
            .unzip();
        let cache = cache_path(dir, &texts);
        let vectors = match cache.as_deref().and_then(read_vectors) {
            Some(v) if v.len() % texts.len().max(1) == 0 => v,
            _ => {
                let refs: Vec<&str> = texts.iter().map(String::as_str).collect();
                let v = embedder.embed(&refs)?;
                if let Some(p) = &cache {
                    let bytes: Vec<u8> = v.iter().flat_map(|x| x.to_le_bytes()).collect();
                    let _ = p.parent().map(fs::create_dir_all);
                    if let Err(e) = fs::write(p, bytes) {
                        eprintln!("voice-assistant: {}: {e}", p.display());
                    }
                }
                v
            }
        };
        let dim = vectors.len() / texts.len().max(1);
        Ok(Classifier {
            embedder,
            texts,
            labels,
            kinds,
            vectors,
            dim,
        })
    }

    pub fn nearest(&mut self, text: &str) -> Result<Option<Nearest>, String> {
        let q = self.embedder.embed(&[text])?;
        let mut best = vec![(f32::NEG_INFINITY, 0usize); self.kinds.len()];
        for (i, v) in self.vectors.chunks_exact(self.dim.max(1)).enumerate() {
            let s: f32 = q.iter().zip(v).map(|(a, b)| a * b).sum();
            let b = &mut best[self.labels[i]];
            if s > b.0 {
                *b = (s, i);
            }
        }
        let mut order: Vec<usize> = (0..best.len()).collect();
        order.sort_by(|&a, &b| best[b].0.total_cmp(&best[a].0));
        let (Some(&top), Some(&second)) = (order.first(), order.get(1)) else {
            return Ok(None);
        };
        let (score, example) = best[top];
        Ok(Some(Nearest {
            intent: self.kinds[top].clone(),
            score,
            lead: score - best[second].0,
            example: self.texts[example].clone(),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brain::{Brain, Shared};
    use crate::clock::Clock;

    fn setup() -> Option<(Classifier, Shared)> {
        let dir = option_env!("VA_INTENT_MODEL")?;
        let b = Shared::new(Brain::connect().ok()?);
        let c = Classifier::new(Path::new(dir), b.examples(&Clock::at(9 * 3600)).ok()?).ok()?;
        Some((c, b))
    }

    /// `-` marks sentences that ask for nothing we do.
    #[test]
    fn recovers_paraphrases_without_false_accepts() -> Result<(), String> {
        let Some((mut c, b)) = setup() else {
            return Ok(());
        };
        let ctx = Clock::at(9 * 3600);
        let (mut right, mut wrong, mut false_accepts) = (Vec::new(), Vec::new(), Vec::new());
        for line in include_str!("eval.tsv").lines() {
            let Some((want, text)) = line.split_once('\t') else {
                continue;
            };
            if b.parse(&[text.to_owned()], &ctx)[0].is_some() {
                continue;
            }
            let Some(n) = c.nearest(text)? else {
                continue;
            };
            if !b.classified(text, &n, &ctx)?.1 {
                continue;
            }
            let got = n.intent;
            match want {
                "-" => false_accepts.push(format!("{text} -> {got}")),
                w if w == got => right.push(text),
                _ => wrong.push(format!("{text} -> {got}, want {want}")),
            }
        }
        // Baseline: "I can't hear it" -> Chat(AreYouThere) is the one wrong.
        assert!(right.len() >= 19, "recovered only {right:?}");
        assert!(wrong.len() <= 1, "{wrong:?}");
        assert!(false_accepts.is_empty(), "{false_accepts:?}");
        Ok(())
    }
}
