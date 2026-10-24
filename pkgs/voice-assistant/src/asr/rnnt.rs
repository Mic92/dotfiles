//! log P(tokens | audio) for a transducer: the forward variable summed
//! over every alignment of the tokens to the frames, as in NeMo's
//! `RNNTLossPytorch.compute_forward_prob`.

fn logaddexp(a: f32, b: f32) -> f32 {
    let m = a.max(b);
    if m == f32::NEG_INFINITY {
        return m;
    }
    m + ((a - m).exp() + (b - m).exp()).ln()
}

/// `blank[t * (u_len + 1) + u]` is the log-prob of moving to the next frame
/// after `u` tokens. `emit[t * u_len + u]` is the log-prob of emitting
/// token `u` there.
pub fn forward(frames: usize, u_len: usize, blank: &[f32], emit: &[f32]) -> f32 {
    if frames == 0 {
        return f32::NEG_INFINITY;
    }
    let w = u_len + 1;
    let mut alpha = vec![f32::NEG_INFINITY; frames * w];
    for t in 0..frames {
        for u in 0..w {
            alpha[t * w + u] = if t == 0 && u == 0 {
                0.0
            } else {
                let stay = if t > 0 {
                    alpha[(t - 1) * w + u] + blank[(t - 1) * w + u]
                } else {
                    f32::NEG_INFINITY
                };
                let step = if u > 0 {
                    alpha[t * w + u - 1] + emit[t * u_len + u - 1]
                } else {
                    f32::NEG_INFINITY
                };
                logaddexp(stay, step)
            };
        }
    }
    alpha[(frames - 1) * w + u_len] + blank[(frames - 1) * w + u_len]
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every path: at each frame emit some of the remaining tokens, then a
    /// blank moves on. The last frame's blank ends the utterance.
    fn brute(t: usize, frames: usize, u: usize, u_len: usize, b: &[f32], e: &[f32]) -> f32 {
        let blank = b[t * (u_len + 1) + u];
        let mut total = if t + 1 == frames {
            if u == u_len { blank } else { f32::NEG_INFINITY }
        } else {
            blank + brute(t + 1, frames, u, u_len, b, e)
        };
        if u < u_len {
            total = logaddexp(
                total,
                e[t * u_len + u] + brute(t, frames, u + 1, u_len, b, e),
            );
        }
        total
    }

    #[test]
    fn matches_sum_over_alignments() {
        let (frames, u_len) = (4, 3);
        // Arbitrary but fixed log-probs.
        let lp = |i: usize| -0.1 - 0.37 * f32::from(u8::try_from(i * 7 % 11).unwrap_or(0));
        let blank: Vec<f32> = (0..frames * (u_len + 1)).map(lp).collect();
        let emit: Vec<f32> = (0..frames * u_len).map(|i| lp(i + 5)).collect();
        let want = brute(0, frames, 0, u_len, &blank, &emit);
        let got = forward(frames, u_len, &blank, &emit);
        assert!((want - got).abs() < 1e-4, "{want} vs {got}");
    }

    #[test]
    fn certain_path_scores_zero() {
        // One frame, one token: emit it (p=1), then blank (p=1).
        assert!(forward(1, 1, &[f32::NEG_INFINITY, 0.0], &[0.0]).abs() < 1e-6);
        assert_eq!(forward(0, 0, &[], &[]), f32::NEG_INFINITY);
    }
}
