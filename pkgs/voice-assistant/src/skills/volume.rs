//! Output volume of the default PipeWire sink via wpctl.

use super::{Action, Context, Intent, Module, Reply, Skill, intent, pick, spawn};
use crate::grammar::Value;

const SINK: &str = "@DEFAULT_AUDIO_SINK@";

#[derive(Debug, Clone, PartialEq)]
pub enum Volume {
    Up,
    Down,
    Set(u32),
    Mute,
    Unmute,
}

pub const MODULE: Module = Module {
    intents,
    slots: &[],
};

pub fn intents() -> Vec<Intent> {
    const WHAT: &str = "[the|my] (volume|sound|music|audio|it)";
    vec![
        intent(
            &[
                &format!("(turn|crank|pump) up {WHAT}"),
                &format!("(turn|crank|pump) {WHAT} up"),
                "(volume|sound) up",
                "make it louder",
                "!louder",
            ],
            |_, _| Some(Action::Volume(Volume::Up)),
        ),
        intent(
            &[
                &format!("turn down {WHAT}"),
                &format!("turn {WHAT} down"),
                "(volume|sound) down",
                "make it (quieter|softer)",
                "!(quieter|softer)",
            ],
            |_, _| Some(Action::Volume(Volume::Down)),
        ),
        intent(
            &[
                "[set|change|turn] [the] volume (to|at) {percent} [percent]",
                "volume {percent} [percent]",
            ],
            |s, _| match s.get("percent")? {
                Value::Number(n) => Some(Action::Volume(Volume::Set(*n))),
                _ => None,
            },
        ),
        intent(
            &["mute [the|my] (sound|audio|volume|speakers|music)", "!mute"],
            |_, _| Some(Action::Volume(Volume::Mute)),
        ),
        intent(
            &[
                "unmute [the|my] (sound|audio|volume|speakers|music)",
                "!unmute",
                // How whisper tends to hear "unmute".
                "!(i'm|im|un|on) mute",
            ],
            |_, _| Some(Action::Volume(Volume::Unmute)),
        ),
    ]
}

const UP: &[&str] = &[
    "Louder it is. Party time!",
    "Cranking it up. The neighbours will love you.",
    "Up we go.",
    "Louder! Let them hear it in the stairwell.",
    "Turning it up. Your ears signed a waiver.",
];
const DOWN: &[&str] = &[
    "Okay, okay, softer.",
    "Turning it down. Getting old?",
    "Quieter. Shh.",
    "Softer. Somebody's got a headache.",
    "Down. Did the neighbours knock again?",
];
const SET: &[&str] = &[
    "Volume {}. As you wish.",
    "{} it is.",
    "Volume at {}. Happy now?",
    "{}. Precise. I like that in a human.",
];
const MUTE: &[&str] = &[
    "Muted. Silence is golden.",
    "Sound off. Ahh, peace.",
    "Muted. Finally some quiet.",
    "Muted. You can hear me sulk now.",
    "Shh. The silence suits you.",
];
const UNMUTE: &[&str] = &[
    "And we're back!",
    "Sound on. Did you miss it?",
    "Unmuted. Let's make some noise.",
    "Sound's back. Let's be loud together.",
];

impl Skill for Volume {
    fn run(&self, _ctx: &Context) -> Reply {
        let v = self;
        // Louder than 150% only distorts.
        let (args, lines, what) = match v {
            Volume::Up => (
                vec!["set-volume", "-l", "1.5", SINK, "10%+"],
                UP,
                String::new(),
            ),
            Volume::Down => (vec!["set-volume", SINK, "10%-"], DOWN, String::new()),
            Volume::Set(n) => (
                vec!["set-volume", "-l", "1.5", SINK],
                SET,
                format!("{n} percent"),
            ),
            Volume::Mute => (vec!["set-mute", SINK, "1"], MUTE, String::new()),
            Volume::Unmute => (vec!["set-mute", SINK, "0"], UNMUTE, String::new()),
        };
        let level = match v {
            Volume::Set(n) => format!("{n}%"),
            _ => String::new(),
        };
        let mut args = args;
        if !level.is_empty() {
            args.push(&level);
        }
        spawn("wpctl", &args);
        Reply::new("Volume", &what, pick(lines, &what))
    }

    fn repeatable(&self) -> bool {
        matches!(self, Volume::Up | Volume::Down)
    }
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    fn vol(text: &str) -> Option<Volume> {
        match parse_at(text, &Context::at(0))? {
            Action::Volume(v) => Some(v),
            _ => None,
        }
    }

    #[test]
    fn phrasings() {
        for (text, want) in [
            ("Turn up the volume.", Volume::Up),
            ("Turn it up!", Volume::Up),
            ("Louder.", Volume::Up),
            ("Volume up", Volume::Up),
            ("Turn the music down.", Volume::Down),
            ("Quieter, please.", Volume::Down),
            ("Set the volume to 40 percent.", Volume::Set(40)),
            ("Volume 70%", Volume::Set(70)),
            ("Set volume to fifty", Volume::Set(50)),
            ("Mute.", Volume::Mute),
            ("Mute the speakers", Volume::Mute),
            ("Unmute the sound", Volume::Unmute),
            ("I'm mute.", Volume::Unmute),
            ("Un mute", Volume::Unmute),
        ] {
            assert_eq!(vol(text), Some(want), "{text:?}");
        }
        assert_eq!(vol("Set the volume to 500 percent"), None);
    }
}
