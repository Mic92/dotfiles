//! Speech-to-text with NVIDIA's parakeet-unified-en-0.6b (RNNT, int8
//! ONNX export from sherpa-onnx), and scoring candidate texts against the
//! same audio for `rescore`. Silero VAD lives here too: both run on
//! onnxruntime.

mod bpe;
mod features;
mod onnx;
mod rnnt;
mod vad;

use std::fs;
use std::path::{Path, PathBuf};

use ort::session::Session;

use bpe::Bpe;
use features::{Features, N_MELS};
pub use vad::Vad;

/// Tokens one frame may emit before the decoder must move on.
const MAX_SYMBOLS: usize = 10;
const PRED_LAYERS: usize = 2;
const PRED_HIDDEN: usize = 640;

struct Model {
    encoder: Session,
    decoder: Session,
    joiner: Session,
    bpe: Bpe,
    features: Features,
    blank: usize,
}

/// Encoder output, `[dim][frames]`.
struct Encoded {
    data: Vec<f32>,
    dim: usize,
    frames: usize,
}

/// Prediction network output for one token, with its LSTM state.
struct Pred {
    out: Vec<f32>,
    h: Vec<f32>,
    c: Vec<f32>,
}

fn int<T: TryFrom<usize>>(n: usize) -> Result<T, String> {
    T::try_from(n).map_err(|_| format!("{n} out of range"))
}

fn log_sum_exp(logits: &[f32]) -> f32 {
    let m = logits.iter().copied().fold(f32::NEG_INFINITY, f32::max);
    m + logits.iter().map(|l| (l - m).exp()).sum::<f32>().ln()
}

impl Model {
    fn load(dir: &Path, threads: usize) -> Result<Self, String> {
        let tokens = dir.join("tokens.txt");
        let bpe = Bpe::parse(
            &fs::read_to_string(&tokens).map_err(|e| format!("{}: {e}", tokens.display()))?,
        )?;
        let blank = bpe.len() - 1;
        Ok(Model {
            encoder: onnx::session(&dir.join("encoder.int8.onnx"), threads)?,
            decoder: onnx::session(&dir.join("decoder.int8.onnx"), 1)?,
            joiner: onnx::session(&dir.join("joiner.int8.onnx"), 1)?,
            bpe,
            features: Features::new(),
            blank,
        })
    }

    fn encode(&mut self, samples: &[f32]) -> Result<Encoded, String> {
        let (feats, frames) = self.features.compute(samples);
        // [frames][mel] -> [1][mel][frames]
        let mut input = vec![0f32; feats.len()];
        for t in 0..frames {
            for m in 0..N_MELS {
                input[m * frames + t] = feats[t * N_MELS + m];
            }
        }
        let out = self
            .encoder
            .run(ort::inputs![
                "audio_signal" => onnx::tensor(&[1, N_MELS, frames], input)?,
                "length" => onnx::tensor(&[1], vec![int::<i64>(frames)?])?,
            ])
            .map_err(|e| format!("encoder: {e}"))?;
        let (shape, data) = onnx::output(&out, "outputs")?;
        let [1, dim, frames] = shape[..] else {
            return Err(format!("encoder output shape {shape:?}"));
        };
        Ok(Encoded {
            data: data.to_vec(),
            dim,
            frames,
        })
    }

    /// Run the prediction network over `tokens` from `state` (zero if none).
    fn predict(
        &mut self,
        tokens: &[usize],
        state: Option<(&[f32], &[f32])>,
    ) -> Result<Pred, String> {
        let zero = vec![0f32; PRED_LAYERS * PRED_HIDDEN];
        let (h, c) = state.unwrap_or((&zero, &zero));
        let targets = tokens
            .iter()
            .map(|&t| int::<i32>(t))
            .collect::<Result<_, _>>()?;
        let out = self
            .decoder
            .run(ort::inputs![
                "targets" => onnx::tensor(&[1, tokens.len()], targets)?,
                "target_length" => onnx::tensor(&[1], vec![int::<i32>(tokens.len())?])?,
                "states.1" => onnx::tensor(&[PRED_LAYERS, 1, PRED_HIDDEN], h.to_vec())?,
                "onnx::Slice_3" => onnx::tensor(&[PRED_LAYERS, 1, PRED_HIDDEN], c.to_vec())?,
            ])
            .map_err(|e| format!("decoder: {e}"))?;
        Ok(Pred {
            out: onnx::output(&out, "outputs")?.1.to_vec(),
            h: onnx::output(&out, "states")?.1.to_vec(),
            c: onnx::output(&out, "162")?.1.to_vec(),
        })
    }

    /// Joint logits `[frames][preds][vocab]` for encoder frames
    /// `[dim][frames]` and prediction outputs `[PRED_HIDDEN][preds]`.
    fn join(
        &mut self,
        enc: Vec<f32>,
        dim: usize,
        frames: usize,
        pred: Vec<f32>,
        preds: usize,
    ) -> Result<Vec<f32>, String> {
        let out = self
            .joiner
            .run(ort::inputs![
                "encoder_outputs" => onnx::tensor(&[1, dim, frames], enc)?,
                "decoder_outputs" => onnx::tensor(&[1, PRED_HIDDEN, preds], pred)?,
            ])
            .map_err(|e| format!("joiner: {e}"))?;
        Ok(onnx::output(&out, "outputs")?.1.to_vec())
    }

    fn greedy(&mut self, enc: &Encoded) -> Result<Vec<usize>, String> {
        let mut pred = self.predict(&[self.blank], None)?;
        let mut out = Vec::new();
        for t in 0..enc.frames {
            let frame: Vec<f32> = (0..enc.dim).map(|d| enc.data[d * enc.frames + t]).collect();
            for _ in 0..MAX_SYMBOLS {
                let logits = self.join(frame.clone(), enc.dim, 1, pred.out.clone(), 1)?;
                let best = (0..logits.len())
                    .max_by(|&a, &b| logits[a].total_cmp(&logits[b]))
                    .unwrap_or(self.blank);
                if best == self.blank {
                    break;
                }
                out.push(best);
                pred = self.predict(&[best], Some((&pred.h, &pred.c)))?;
            }
        }
        Ok(out)
    }

    fn score(&mut self, enc: &Encoded, text: &str) -> Result<f32, String> {
        let tokens = self.bpe.encode(text);
        let u_len = tokens.len();
        let mut inputs = vec![self.blank];
        inputs.extend(&tokens);
        let pred = self.predict(&inputs, None)?;
        let vocab = self.bpe.len();
        let logits = self.join(enc.data.clone(), enc.dim, enc.frames, pred.out, u_len + 1)?;
        if logits.len() != enc.frames * (u_len + 1) * vocab {
            return Err(format!("joiner returned {} logits", logits.len()));
        }
        let mut blank = Vec::with_capacity(enc.frames * (u_len + 1));
        let mut emit = Vec::with_capacity(enc.frames * u_len);
        for t in 0..enc.frames {
            for u in 0..=u_len {
                let cell = &logits[(t * (u_len + 1) + u) * vocab..][..vocab];
                let lse = log_sum_exp(cell);
                blank.push(cell[self.blank] - lse);
                if let Some(&token) = tokens.get(u) {
                    emit.push(cell[token] - lse);
                }
            }
        }
        Ok(rnnt::forward(enc.frames, u_len, &blank, &emit))
    }
}

/// Speech-to-text model, loaded on the first command and then kept in RAM.
pub struct Asr {
    dir: PathBuf,
    threads: usize,
    model: Option<Model>,
    last: Option<Encoded>,
}

impl Asr {
    pub fn new(dir: PathBuf, threads: usize) -> Self {
        Asr {
            dir,
            threads: threads.max(1),
            model: None,
            last: None,
        }
    }

    fn model(&mut self) -> Result<&mut Model, String> {
        if self.model.is_none() {
            self.model = Some(Model::load(&self.dir, self.threads)?);
        }
        self.model
            .as_mut()
            .ok_or_else(|| "speech model missing".into())
    }

    pub fn transcribe(&mut self, samples: &[f32]) -> Result<String, String> {
        self.last = None;
        let model = self.model()?;
        let enc = model.encode(samples)?;
        let ids = model.greedy(&enc)?;
        let text = model.bpe.decode(&ids);
        self.last = Some(enc);
        Ok(text)
    }

    /// Log-probability of each candidate text given the audio of the last
    /// `transcribe`.
    pub fn score(&mut self, candidates: &[String]) -> Result<Vec<f32>, String> {
        let enc = self
            .last
            .take()
            .ok_or("nothing transcribed to score against")?;
        let model = self.model()?;
        let scores = candidates.iter().map(|c| model.score(&enc, c)).collect();
        self.last = Some(enc);
        scores
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Runs where the build provides the model (the nix build does).
    fn asr() -> Option<Asr> {
        option_env!("VA_ASR_MODEL").map(|d| Asr::new(d.into(), 4))
    }

    fn wav(name: &str) -> Option<Vec<f32>> {
        let dir = PathBuf::from(option_env!("VA_ASR_MODEL")?);
        crate::replay::read_wav(&dir.join("test_wavs").join(name))
    }

    #[test]
    fn transcribes_and_scores_its_own_test_audio() -> Result<(), String> {
        let (Some(mut asr), Some(audio)) = (asr(), wav("0.wav")) else {
            return Ok(());
        };
        let text = asr.transcribe(&audio)?;
        assert!(text.to_lowercase().contains("observed"), "{text}");
        let wrong = text.replace("observed", "absurd");
        let s = asr.score(&[text.clone(), wrong])?;
        assert!(s[0] > s[1] + 3.0, "{text:?}: {s:?}");
        assert!(s[0] > -10.0, "{s:?}");
        Ok(())
    }
}
