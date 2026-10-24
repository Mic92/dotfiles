//! Files the assistant leaves behind: state directory, TSV logs, WAV clips.

use std::io::Write;
use std::path::{Path, PathBuf};

use crate::RATE_HZ;
use std::env;
use std::fs;
use std::fs::OpenOptions;
use std::io;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

/// `$XDG_STATE_HOME/voice-assistant`.
pub fn state_dir() -> Option<PathBuf> {
    let base = env::var_os("XDG_STATE_HOME")
        .map(PathBuf::from)
        .or_else(|| env::var_os("HOME").map(|h| PathBuf::from(h).join(".local/state")))?;
    Some(base.join("voice-assistant"))
}

/// Milliseconds since the epoch; 0 if the clock is before 1970.
pub fn unix_ms() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| d.as_millis())
}

/// Append one line, creating the file and its directories.
pub fn append(path: &Path, line: &str) -> io::Result<()> {
    if let Some(dir) = path.parent() {
        fs::create_dir_all(dir)?;
    }
    OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)?
        .write_all(format!("{line}\n").as_bytes())
}

/// Tab-separated row; tabs and newlines inside fields become spaces.
pub fn tsv(fields: &[&str]) -> String {
    fields
        .iter()
        .map(|f| f.replace(['\t', '\n'], " "))
        .collect::<Vec<_>>()
        .join("\t")
}

pub fn to_pcm(audio: &[f32]) -> Vec<i16> {
    // Clamped to the i16 range first, so the cast cannot truncate.
    #[allow(clippy::cast_possible_truncation)]
    audio
        .iter()
        .map(|s| (s.clamp(-1.0, 1.0) * 32767.0) as i16)
        .collect()
}

/// 16 kHz mono 16-bit WAV, creating parent directories.
pub fn write_wav(path: &Path, samples: &[i16]) -> io::Result<()> {
    let data_len = u32::try_from(samples.len() * 2)
        .map_err(|_| io::Error::other("recording too long for WAV"))?;
    let mut wav = Vec::with_capacity(44 + samples.len() * 2);
    wav.extend_from_slice(b"RIFF");
    wav.extend_from_slice(&(36 + data_len).to_le_bytes());
    wav.extend_from_slice(b"WAVEfmt ");
    for v in [16u32, 1 | (1 << 16), RATE_HZ, RATE_HZ * 2, 2 | (16 << 16)] {
        wav.extend_from_slice(&v.to_le_bytes());
    }
    wav.extend_from_slice(b"data");
    wav.extend_from_slice(&data_len.to_le_bytes());
    wav.extend(samples.iter().flat_map(|s| s.to_le_bytes()));
    if let Some(dir) = path.parent() {
        fs::create_dir_all(dir)?;
    }
    fs::write(path, wav)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::fs;
    use std::process;

    #[test]
    fn append_creates_and_appends() {
        let d = env::temp_dir().join(format!("va-store-{}-{}", process::id(), unix_ms()));
        let f = d.join("a/b.tsv");
        assert!(append(&f, &tsv(&["1", "x\ty", "z\n"])).is_ok());
        assert!(append(&f, "2").is_ok());
        assert_eq!(
            fs::read_to_string(&f).ok().as_deref(),
            Some("1\tx y\tz \n2\n")
        );
        let wav = d.join("c.wav");
        assert!(write_wav(&wav, &[0, 1, -1]).is_ok());
        assert_eq!(fs::metadata(&wav).map(|m| m.len()).ok(), Some(50));
        let _ = fs::remove_dir_all(d);
    }
}
