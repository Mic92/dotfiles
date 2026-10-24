//! Open applications from a fixed list.

use super::{Action, Context, Intent, Module, Reply, Skill, intent, pick, spawn};
use crate::grammar::Value;

struct App {
    /// Spoken names, each one or more words.
    names: &'static [&'static str],
    /// Shown and spoken.
    label: &'static str,
    cmd: &'static [&'static str],
}

/// Every spoken name of every app.
pub fn names() -> impl Iterator<Item = &'static str> {
    APPS.iter().flat_map(|a| a.names.iter().copied())
}

const APPS: &[App] = &[
    App {
        names: &[
            "browser",
            "web browser",
            "librewolf",
            "firefox",
            "internet",
            "web",
        ],
        label: "the browser",
        cmd: &["librewolf"],
    },
    App {
        names: &["terminal", "shell", "console", "ghostty"],
        label: "a terminal",
        cmd: &["ghostty"],
    },
    App {
        names: &["signal", "messenger", "chat"],
        label: "Signal",
        cmd: &["signal-desktop"],
    },
    App {
        names: &["mail", "email", "e mail", "inbox", "aerc"],
        label: "your mail",
        cmd: &["ghostty", "-e", "aerc"],
    },
    App {
        names: &["files", "file manager", "yazi"],
        label: "the file manager",
        cmd: &["ghostty", "-e", "yazi"],
    },
    App {
        names: &["editor", "text editor", "vim", "neovim", "nvim"],
        label: "the editor",
        cmd: &["ghostty", "-e", "nvim"],
    },
    App {
        names: &["system monitor", "task manager", "htop", "processes"],
        label: "the system monitor",
        cmd: &["ghostty", "-e", "htop"],
    },
    App {
        names: &[
            "sound settings",
            "audio settings",
            "mixer",
            "volume control",
            "pavucontrol",
        ],
        label: "the sound settings",
        cmd: &["pavucontrol"],
    },
    App {
        names: &["gimp", "image editor", "photo editor"],
        label: "GIMP",
        cmd: &["gimp"],
    },
    App {
        names: &["pdf viewer", "document viewer", "evince"],
        label: "the document viewer",
        cmd: &["evince"],
    },
    App {
        names: &["settings", "preferences", "control center"],
        label: "the settings",
        cmd: &["noctalia", "msg", "settings-open"],
    },
];

/// Index of the app named by exactly these words.
pub fn lookup(words: &[&str]) -> Option<usize> {
    let spoken = words.join(" ");
    APPS.iter().position(|a| a.names.contains(&spoken.as_str()))
}

/// Index into `APPS`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Launch(pub usize);

pub const MODULE: Module = Module {
    intents,
    slots: &[("app", |w, _| lookup(w).map(Value::Index))],
};

pub fn intents() -> Vec<Intent> {
    vec![intent(
        &[
            "(open|launch|start|run|show) [the|my|a|an] {app} [app|application|window]",
            "(open|launch|start) [up] [the|my|a|an] {app}",
            "i (need|want) [the|my|a|an] {app}",
        ],
        |s, _| match s.get("app")? {
            Value::Index(i) => Some(Action::App(Launch(*i))),
            _ => None,
        },
    )]
}

const OPENING: &[&str] = &[
    "Opening {}.",
    "{}, coming right up.",
    "Here's {}. Don't say I never do anything for you.",
    "{}. Anything else, your majesty?",
    "{}. Try not to get distracted this time.",
    "Launching {}. I'd say break a leg, but you might.",
    "{}. Opening it before you change your mind.",
];

impl Skill for Launch {
    fn run(&self, _ctx: &Context) -> Reply {
        let i = self.0;
        let Some(app) = APPS.get(i) else {
            return Reply::new("Unknown app", "", "I don't know that app. Awkward.".into());
        };
        if let Some((cmd, args)) = app.cmd.split_first() {
            spawn(cmd, args);
        }
        let speech = pick(OPENING, app.label);
        let mut chars = speech.chars();
        let speech = chars
            .next()
            .map(|c| c.to_uppercase().chain(chars).collect())
            .unwrap_or_default();
        Reply::new("Opening", app.label, speech)
    }
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    #[test]
    fn phrasings() {
        let label = |t| match parse_at(t, &Context::at(0)) {
            Some(Action::App(Launch(i))) => APPS.get(i).map(|a| a.label),
            _ => None,
        };
        for (text, want) in [
            ("Open the browser.", "the browser"),
            ("Open Firefox", "the browser"),
            ("Launch a terminal", "a terminal"),
            ("Open my email.", "your mail"),
            ("Open the file manager", "the file manager"),
            ("Start the system monitor", "the system monitor"),
            ("Open the sound settings", "the sound settings"),
            ("Open Signal.", "Signal"),
            ("I need a terminal", "a terminal"),
        ] {
            assert_eq!(label(text), Some(want), "{text:?}");
        }
        assert_eq!(label("Open the pod bay doors"), None);
    }
}
