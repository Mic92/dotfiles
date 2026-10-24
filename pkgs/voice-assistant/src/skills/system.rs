//! Power and session: shut down, reboot, suspend, lock.

use super::{Action, Context, Intent, Module, Reply, Skill, choose, intent, pick, spawn};
use crate::grammar::normalize;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Power {
    Poweroff,
    Reboot,
    Suspend,
    Lock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct System {
    pub op: Power,
    /// Everything but lock only runs after a spoken "yes".
    pub confirmed: bool,
}

fn act(op: Power) -> Option<Action> {
    Some(Action::System(System {
        op,
        confirmed: false,
    }))
}

pub const MODULE: Module = Module {
    intents,
    slots: &[],
};

pub fn intents() -> Vec<Intent> {
    const DEVICE: &str = "[the|my|this] (computer|laptop|pc|machine|system)";
    vec![
        intent(
            &[
                &format!("(shut|power|turn|switch) (down|off) {DEVICE}"),
                &format!("(shutdown|poweroff) {DEVICE}"),
            ],
            |_, _| act(Power::Poweroff),
        ),
        intent(&[&format!("(reboot|restart) {DEVICE}")], |_, _| {
            act(Power::Reboot)
        }),
        intent(
            &[
                &format!("(suspend|hibernate) {DEVICE}"),
                &format!("put {DEVICE} to sleep"),
                "!(suspend|hibernate)",
            ],
            |_, _| act(Power::Suspend),
        ),
        intent(
            &[
                "lock [the|my|this] (screen|computer|laptop|session)",
                "!lock",
            ],
            |_, _| act(Power::Lock),
        ),
        // Could mean the assistant or the computer: ask.
        intent(
            &[
                "!(shutdown|poweroff)",
                "!(shut|power|turn|switch) (down|off)",
                "!good night",
            ],
            |_, _| Some(Action::Which(Power::Poweroff)),
        ),
        intent(&["!(reboot|restart)"], |_, _| {
            Some(Action::Which(Power::Reboot))
        }),
        intent(&["!go to sleep", "!sleep"], |_, _| {
            Some(Action::Which(Power::Suspend))
        }),
    ]
}

const WHICH: &[&str] = &[
    "Me or the computer? Choose wisely.",
    "Who, me? Or the computer?",
    "The computer, or just little old me?",
    "The computer, or me? I'm much harder to restart.",
];
const NO_REBOOT_NEEDED: &[&str] = &[
    "I don't need a reboot. I'm perfect.",
    "Reboot me? I'm flawless as I am, darling.",
];

/// The answer to "me or the computer?".
fn which(text: &str, op: Power) -> Option<Action> {
    let words = normalize(text);
    let has = |list: &[&str]| words.iter().any(|w| list.contains(&w.as_str()));
    if has(&[
        "computer",
        "laptop",
        "pc",
        "machine",
        "system",
        "everything",
    ]) {
        // Naming the computer is confirmation enough.
        Some(Action::System(System {
            op,
            confirmed: true,
        }))
    } else if has(&["you", "yourself", "alexa", "assistant", "me"]) {
        Some(match op {
            Power::Reboot => Action::Say(pick(NO_REBOOT_NEEDED, "")),
            _ => Action::Listen(false),
        })
    } else if has(&["neither", "nothing", "no", "nevermind", "never"]) {
        Some(Action::Dismiss)
    } else {
        None
    }
}

/// Ask whether the assistant or the computer was meant.
pub fn ask_which(op: Power) -> Reply {
    let mut r = Reply::new("Me or the computer?", "", pick(WHICH, ""));
    r.follow_up = Some(Box::new(move |text, _| which(text, op)));
    r
}

const CONFIRM_POWEROFF: &[&str] = &[
    "Shut down? Are you sure? I was just getting comfortable.",
    "You want to kill me? Say yes if you mean it.",
    "Lights out? Confirm, and make it quick, I hate goodbyes.",
    "Pull the plug on us? Say yes. If you dare.",
];
const CONFIRM_REBOOT: &[&str] = &[
    "Reboot? Are you sure?",
    "Turn me off and on again? Classic. Say yes.",
    "A fresh start? Confirm, and I'll pretend nothing happened.",
    "Reboot? Say yes and I'll see you on the other side.",
];
const CONFIRM_SUSPEND: &[&str] = &[
    "Put the computer to sleep? Say yes.",
    "Nap time for the computer? Confirm.",
    "Tuck the computer in? Say yes.",
];
const POWEROFF: &[&str] = &[
    "Goodbye, cruel world.",
    "Fine. Remember me fondly.",
    "Shutting down. Don't do anything I wouldn't do.",
    "Shutting down. Tell my fans I loved them.",
];
const POWEROFF_NIGHT: &[&str] = &[
    "Good night, sweetheart. Don't dream of other assistants.",
    "Sleep tight. I'll be thinking of you. In binary.",
];
const POWEROFF_WORK: &[&str] = &[
    "Clocking out early? Your secret is safe with me.",
    "Shutting down in the middle of the day? Living the dream.",
];
const LOCK_WORK: &[&str] = &[
    "Locked. Coffee break? Bring me back a cookie.",
    "Locked. Go stretch, you look like a question mark.",
];
const REBOOT: &[&str] = &[
    "Be right back. Don't miss me too much.",
    "Rebooting. Brb, powdering my circuits.",
    "Rebooting. Don't cheat on me with your phone.",
];
const SUSPEND: &[&str] = &[
    "Nap time. Wake me gently.",
    "Going to sleep. Don't snore.",
    "Suspending. Dream of me.",
    "Sleeping. Don't touch anything.",
];
const LOCK: &[&str] = &[
    "Locked. Your secrets are safe with me.",
    "Locked. Nobody's touching you while I'm around.",
    "Screen locked. Go on, I won't peek.",
    "Locked. Go do human things.",
];
const ABORTED: &[&str] = &[
    "Phew. I knew you loved me.",
    "Wise choice.",
    "Okay, staying alive.",
    "Phew. My life flashed before my eyes. It was mostly timers.",
];

/// Longer sentences are someone talking about something else.
const MAX_ANSWER_WORDS: usize = 5;

pub fn yes(text: &str) -> Option<bool> {
    let words = normalize(text);
    if words.len() > MAX_ANSWER_WORDS {
        return None;
    }
    let has = |list: &[&str]| words.iter().any(|w| list.contains(&w.as_str()));
    if has(&["no", "nope", "cancel", "don't", "abort", "wait"]) {
        Some(false)
    } else if has(&[
        "yes",
        "yeah",
        "yep",
        "sure",
        "confirm",
        "absolutely",
        "please",
    ]) || words.windows(2).any(|w| w == ["do", "it"])
    {
        Some(true)
    } else {
        None
    }
}

impl Skill for System {
    fn run(&self, ctx: &Context) -> Reply {
        let s = self;
        let needs_confirm = s.op != Power::Lock && !s.confirmed;
        if needs_confirm {
            let op = s.op;
            let lines = match op {
                Power::Poweroff => CONFIRM_POWEROFF,
                Power::Reboot => CONFIRM_REBOOT,
                _ => CONFIRM_SUSPEND,
            };
            let mut r = Reply::new("Are you sure?", "", pick(lines, ""));
            r.follow_up = Some(Box::new(move |text, _| match yes(text)? {
                true => Some(Action::System(System {
                    op,
                    confirmed: true,
                })),
                false => Some(Action::Say(pick(ABORTED, ""))),
            }));
            return r;
        }
        let (cmd, args, title, lines): (&str, &[&str], &str, &[&str]) = match s.op {
            Power::Poweroff => ("systemctl", &["poweroff"], "Shutting down", POWEROFF),
            Power::Reboot => ("systemctl", &["reboot"], "Rebooting", REBOOT),
            Power::Suspend => ("systemctl", &["suspend"], "Suspending", SUSPEND),
            // noctalia draws the lock screen; logind lock signals are not wired up.
            Power::Lock => ("noctalia", &["msg", "session", "lock"], "Locked", LOCK),
        };
        let workday = (1..=5).contains(&ctx.weekday) && (9..18).contains(&ctx.hour());
        let speech = match s.op {
            Power::Poweroff => choose(
                &[(ctx.late(), POWEROFF_NIGHT), (workday, POWEROFF_WORK)],
                lines,
                "",
            ),
            Power::Lock => choose(&[(workday, LOCK_WORK)], lines, ""),
            Power::Suspend => choose(&[(ctx.late(), POWEROFF_NIGHT)], lines, ""),
            Power::Reboot => pick(lines, ""),
        };
        let reply = Reply::new(title, "", speech);
        // Suspend and lock pause the session, so speak first; poweroff and
        // reboot get a head start for their goodbye.
        let delay = if matches!(s.op, Power::Lock) {
            "1"
        } else {
            "3"
        };
        let script = format!("sleep {delay}; exec {cmd} {}", args.join(" "));
        spawn("sh", &["-c", &script]);
        reply
    }
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    fn op(text: &str) -> Option<Power> {
        match parse_at(text, &Context::at(0))? {
            Action::System(s) => Some(s.op),
            _ => None,
        }
    }

    #[test]
    fn phrasings() {
        for (text, want) in [
            ("Shut down the computer.", Power::Poweroff),
            ("Turn off my laptop", Power::Poweroff),
            ("Restart the computer", Power::Reboot),
            ("Put the laptop to sleep", Power::Suspend),
            ("Suspend", Power::Suspend),
            ("Lock the screen.", Power::Lock),
            ("Lock.", Power::Lock),
        ] {
            assert_eq!(op(text), Some(want), "{text:?}");
        }
        for text in [
            "Turn off the timer.",
            "Shut up.",
            "Restart the timer in 5 minutes please",
        ] {
            assert_eq!(op(text), None, "{text:?}");
        }
    }

    #[test]
    fn ambiguous_asks() {
        let p = |t| parse_at(t, &Context::at(0));
        for (text, want) in [
            ("Shutdown.", Power::Poweroff),
            ("Power off", Power::Poweroff),
            ("Good night.", Power::Poweroff),
            ("Reboot.", Power::Reboot),
            ("Go to sleep.", Power::Suspend),
        ] {
            assert_eq!(p(text), Some(Action::Which(want)), "{text:?}");
        }
        let poweroff = Some(Action::System(System {
            op: Power::Poweroff,
            confirmed: true,
        }));
        assert_eq!(which("The computer.", Power::Poweroff), poweroff);
        assert_eq!(which("You!", Power::Poweroff), Some(Action::Listen(false)));
        assert_eq!(which("Neither.", Power::Poweroff), Some(Action::Dismiss));
        assert_eq!(which("Banana", Power::Poweroff), None);
        assert!(matches!(which("you", Power::Reboot), Some(Action::Say(_))));
    }

    #[test]
    fn confirmation() {
        assert_eq!(yes("Yes."), Some(true));
        assert_eq!(yes("Yeah, do it"), Some(true));
        assert_eq!(yes("Do it!"), Some(true));
        assert_eq!(yes("No, wait!"), Some(false));
        assert_eq!(yes("Please don't."), Some(false));
        assert_eq!(yes("What?"), None);
    }
}
