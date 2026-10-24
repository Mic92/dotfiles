//! Silero VAD v5: one speech probability per 32 ms of 16 kHz audio.

use std::path::Path;

use ort::session::Session;

use super::onnx;

pub const FRAME: usize = 512;
/// Silero v5 sees the tail of the previous frame in front of each one.
const CONTEXT: usize = 64;
const STATE: usize = 2 * 128;

pub struct Vad {
    session: Session,
    state: Vec<f32>,
    context: Vec<f32>,
    pending: Vec<f32>,
}

impl Vad {
    pub fn new(model: &Path) -> Result<Self, String> {
        Ok(Vad {
            session: onnx::session(model, 1)?,
            state: vec![0.0; STATE],
            context: vec![0.0; CONTEXT],
            pending: Vec::new(),
        })
    }

    pub fn reset(&mut self) {
        self.state.fill(0.0);
        self.context.fill(0.0);
        self.pending.clear();
    }

    fn frame(&mut self, frame: &[f32]) -> Result<f32, String> {
        let input: Vec<f32> = self.context.iter().chain(frame).copied().collect();
        self.context.copy_from_slice(&frame[FRAME - CONTEXT..]);
        let out = self
            .session
            .run(ort::inputs![
                "input" => onnx::tensor(&[1, CONTEXT + FRAME], input)?,
                "state" => onnx::tensor(&[2, 1, 128], self.state.clone())?,
                "sr" => onnx::tensor::<i64>(&[], vec![16000])?,
            ])
            .map_err(|e| format!("vad: {e}"))?;
        let (_, state) = onnx::output(&out, "stateN")?;
        self.state.copy_from_slice(state);
        let (_, p) = onnx::output(&out, "output")?;
        p.first().copied().ok_or_else(|| "vad: no output".into())
    }

    /// Feed samples. Returns one speech probability per completed frame.
    pub fn feed(&mut self, samples: &[f32]) -> Vec<f32> {
        self.pending.extend_from_slice(samples);
        let frames: Vec<[f32; FRAME]> = self.pending.as_chunks::<FRAME>().0.to_vec();
        self.pending.drain(..frames.len() * FRAME);
        frames
            .iter()
            .filter_map(|f| {
                self.frame(f)
                    .inspect_err(|e| eprintln!("voice-assistant: {e}"))
                    .ok()
            })
            .collect()
    }
}
