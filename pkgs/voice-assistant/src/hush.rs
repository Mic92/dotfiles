//! Pause whatever is playing while we listen and talk, so whisper hears the
//! user rather than the song, and resume it afterwards.

use std::env;
use std::io::{BufRead, BufReader, Write};
use std::os::unix::net::UnixStream;
use std::path::PathBuf;
use std::time::Duration;

use crate::exec::output;

#[derive(Debug, Default)]
pub struct Hushed {
    /// MPRIS players we paused.
    players: Vec<String>,
    /// Our own radio, which is not an MPRIS player.
    radio: bool,
}

/// Players reported as playing by `playerctl -a -f '{{playerName}}\t{{status}}' status`.
fn playing(listing: &str) -> Vec<String> {
    listing
        .lines()
        .filter_map(|l| l.split_once('\t'))
        .filter(|(_, status)| status.trim() == "Playing")
        .map(|(name, _)| name.trim().to_owned())
        .filter(|n| !n.is_empty())
        .collect()
}

fn radio_socket() -> Option<PathBuf> {
    env::var_os("XDG_RUNTIME_DIR").map(|d| PathBuf::from(d).join("voice-assistant-radio.sock"))
}

/// One request over mpv's JSON IPC; returns its reply line.
fn mpv_ipc(cmd: &str) -> Option<String> {
    let mut sock = UnixStream::connect(radio_socket()?).ok()?;
    let timeout = Some(Duration::from_millis(300));
    sock.set_read_timeout(timeout).ok()?;
    sock.set_write_timeout(timeout).ok()?;
    writeln!(sock, "{{\"command\": {cmd}, \"request_id\": 1}}").ok()?;
    // Event lines may come first.
    BufReader::new(sock)
        .lines()
        .map_while(Result::ok)
        .take(20)
        .find(|l| l.contains("\"request_id\":1"))
}

/// Pause or resume our radio. When pausing, true if it was playing.
fn radio_pause(pause: bool) -> bool {
    if pause {
        let playing =
            mpv_ipc(r#"["get_property", "pause"]"#).is_some_and(|r| r.contains("\"data\":false"));
        playing && mpv_ipc(r#"["set_property", "pause", true]"#).is_some()
    } else {
        mpv_ipc(r#"["set_property", "pause", false]"#).is_some()
    }
}

pub fn hush() -> Hushed {
    let listing = output(
        "playerctl",
        &["-a", "-f", "{{playerName}}\t{{status}}", "status"],
    )
    .unwrap_or_default();
    let players = playing(&listing);
    for p in &players {
        output("playerctl", &["-p", p, "pause"]);
    }
    Hushed {
        players,
        radio: radio_pause(true),
    }
}

impl Hushed {
    /// Whether anything was playing before we paused it.
    pub fn any(&self) -> bool {
        self.radio || !self.players.is_empty()
    }

    pub fn resume(self) {
        for p in &self.players {
            output("playerctl", &["-p", p, "play"]);
        }
        if self.radio {
            radio_pause(false);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_playing() {
        let l =
            "spotify\tPlaying\nfirefox.instance_1_42\tPaused\nmpv\tPlaying\n\tPlaying\ngarbage\n";
        assert_eq!(playing(l), ["spotify", "mpv"]);
        assert!(playing("").is_empty());
    }
}
