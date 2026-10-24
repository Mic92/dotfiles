//! Pause whatever is playing while we listen and talk, so whisper hears the
//! user rather than the song, and resume it afterwards.

use crate::skills::{media, output};

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
        radio: media::radio_pause(true),
    }
}

impl Hushed {
    pub fn resume(self) {
        for p in &self.players {
            output("playerctl", &["-p", p, "play"]);
        }
        if self.radio {
            media::radio_pause(false);
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
