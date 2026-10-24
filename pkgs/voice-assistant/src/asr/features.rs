//! NeMo's log-mel spectrogram, as parakeet was trained on: pre-emphasis,
//! 25 ms Hann window every 10 ms, 512-point FFT, 128 Slaney mel bands,
//! log, then each band normalised over the utterance. Follows
//! parakeet-rs's `audio.rs` (MIT/Apache-2.0).

use std::f64::consts::PI;

pub const N_MELS: usize = 128;
const N_FFT: usize = 512;
const HOP: usize = 160;
const WIN: usize = 400;
const PREEMPHASIS: f32 = 0.97;
const RATE: f64 = 16000.0;

fn hz_to_mel(hz: f64) -> f64 {
    const F_SP: f64 = 200.0 / 3.0;
    if hz < 1000.0 {
        hz / F_SP
    } else {
        1000.0 / F_SP + (hz / 1000.0).ln() / 0.068_751_777_420_949_12
    }
}

fn mel_to_hz(mel: f64) -> f64 {
    const F_SP: f64 = 200.0 / 3.0;
    let min_log_mel = 1000.0 / F_SP;
    if mel < min_log_mel {
        mel * F_SP
    } else {
        1000.0 * ((mel - min_log_mel) * 0.068_751_777_420_949_12).exp()
    }
}

#[allow(clippy::cast_precision_loss)]
fn to_f64(n: usize) -> f64 {
    n as f64
}

#[allow(clippy::cast_precision_loss)]
fn to_f32(n: usize) -> f32 {
    n as f32
}

pub struct Features {
    /// `N_MELS` rows of `N_FFT / 2 + 1` weights.
    mel: Vec<Vec<f32>>,
    window: Vec<f32>,
    /// `e^(-2πik/N_FFT)` for k < N_FFT / 2.
    twiddle: Vec<(f32, f32)>,
}

/// In-place radix-2 FFT of `N_FFT` complex values.
fn fft(re: &mut [f32], im: &mut [f32], twiddle: &[(f32, f32)]) {
    let n = re.len();
    let mut j = 0;
    for i in 1..n {
        let mut bit = n >> 1;
        while j & bit != 0 {
            j ^= bit;
            bit >>= 1;
        }
        j |= bit;
        if i < j {
            re.swap(i, j);
            im.swap(i, j);
        }
    }
    let mut len = 2;
    while len <= n {
        let step = n / len;
        for start in (0..n).step_by(len) {
            for k in 0..len / 2 {
                let (wr, wi) = twiddle[k * step];
                let (a, b) = (start + k, start + k + len / 2);
                let tr = re[b] * wr - im[b] * wi;
                let ti = re[b] * wi + im[b] * wr;
                re[b] = re[a] - tr;
                im[b] = im[a] - ti;
                re[a] += tr;
                im[a] += ti;
            }
        }
        len <<= 1;
    }
}

impl Features {
    pub fn new() -> Self {
        let bins = N_FFT / 2 + 1;
        let (lo, hi) = (hz_to_mel(0.0), hz_to_mel(RATE / 2.0));
        let pts: Vec<f64> = (0..=N_MELS + 1)
            .map(|i| mel_to_hz(lo + (hi - lo) * to_f64(i) / to_f64(N_MELS + 1)))
            .collect();
        #[allow(clippy::cast_possible_truncation)]
        let mel = (0..N_MELS)
            .map(|i| {
                let norm = 2.0 / (pts[i + 2] - pts[i]);
                (0..bins)
                    .map(|k| {
                        let f = to_f64(k) * RATE / to_f64(N_FFT);
                        let up = (f - pts[i]) / (pts[i + 1] - pts[i]);
                        let down = (pts[i + 2] - f) / (pts[i + 2] - pts[i + 1]);
                        (up.min(down).max(0.0) * norm) as f32
                    })
                    .collect()
            })
            .collect();
        #[allow(clippy::cast_possible_truncation)]
        let window = (0..WIN)
            .map(|i| (0.5 - 0.5 * (2.0 * PI * to_f64(i) / to_f64(WIN - 1)).cos()) as f32)
            .collect();
        #[allow(clippy::cast_possible_truncation)]
        let twiddle = (0..N_FFT / 2)
            .map(|k| {
                let a = -2.0 * PI * to_f64(k) / to_f64(N_FFT);
                (a.cos() as f32, a.sin() as f32)
            })
            .collect();
        Features {
            mel,
            window,
            twiddle,
        }
    }

    /// `frames x N_MELS`, row-major, one frame per 10 ms.
    pub fn compute(&self, audio: &[f32]) -> (Vec<f32>, usize) {
        let mut x = Vec::with_capacity(audio.len() + N_FFT);
        x.resize(N_FFT / 2, 0.0);
        x.push(audio.first().copied().unwrap_or(0.0));
        x.extend(audio.windows(2).map(|w| w[1] - PREEMPHASIS * w[0]));
        x.resize(x.len() + N_FFT / 2, 0.0);
        // NeMo keeps floor(len / hop) frames, dropping torch.stft's last one.
        let frames = audio.len() / HOP;
        let off = (N_FFT - WIN) / 2;
        let mut out = vec![0f32; frames * N_MELS];
        let (mut re, mut im) = (vec![0f32; N_FFT], vec![0f32; N_FFT]);
        let mut power = vec![0f32; N_FFT / 2 + 1];
        for t in 0..frames {
            re.fill(0.0);
            im.fill(0.0);
            for (i, w) in self.window.iter().enumerate() {
                re[off + i] = x[t * HOP + off + i] * w;
            }
            fft(&mut re, &mut im, &self.twiddle);
            for (k, p) in power.iter_mut().enumerate() {
                *p = re[k] * re[k] + im[k] * im[k];
            }
            for (m, weights) in self.mel.iter().enumerate() {
                let e: f32 = weights.iter().zip(&power).map(|(w, p)| w * p).sum();
                out[t * N_MELS + m] = (e + 2f32.powi(-24)).ln();
            }
        }
        if frames > 1 {
            let n = to_f32(frames);
            for m in 0..N_MELS {
                let col = || (0..frames).map(|t| out[t * N_MELS + m]);
                let mean = col().sum::<f32>() / n;
                let var = col().map(|v| (v - mean).powi(2)).sum::<f32>() / (n - 1.0);
                let std = var.sqrt() + 1e-5;
                for t in 0..frames {
                    out[t * N_MELS + m] = (out[t * N_MELS + m] - mean) / std;
                }
            }
        }
        (out, frames)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fft_matches_dft() {
        let f = Features::new();
        let x: Vec<f32> = (0..N_FFT)
            .map(|i| to_f32(i * 37 % 101) / 50.0 - 1.0)
            .collect();
        let (mut re, mut im) = (x.clone(), vec![0f32; N_FFT]);
        fft(&mut re, &mut im, &f.twiddle);
        for k in [0, 1, 7, 100, 256] {
            let (mut dr, mut di) = (0f64, 0f64);
            for (n, v) in x.iter().enumerate() {
                let a = -2.0 * PI * to_f64(k * n % N_FFT) / to_f64(N_FFT);
                dr += f64::from(*v) * a.cos();
                di += f64::from(*v) * a.sin();
            }
            assert!(
                (f64::from(re[k]) - dr).abs() < 1e-3,
                "{k}: {} vs {dr}",
                re[k]
            );
            assert!(
                (f64::from(im[k]) - di).abs() < 1e-3,
                "{k}: {} vs {di}",
                im[k]
            );
        }
    }

    #[test]
    fn one_frame_per_hop_and_normalised() {
        let tone: Vec<f32> = (0..16000)
            .map(|i| (2.0 * std::f32::consts::PI * 1000.0 * to_f32(i) / 16000.0).sin())
            .collect();
        let (f, frames) = Features::new().compute(&tone);
        assert_eq!(frames, 100);
        assert_eq!(f.len(), 100 * N_MELS);
        let band: Vec<f32> = (0..frames).map(|t| f[t * N_MELS + 40]).collect();
        let mean = band.iter().sum::<f32>() / 100.0;
        assert!(mean.abs() < 1e-3, "{mean}");
    }
}
