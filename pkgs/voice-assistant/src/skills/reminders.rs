//! Spoken reminders as transient systemd user timers, so they survive
//! the assistant restarting.

use super::json::{key, text as json_text};
use super::notes::spoken_list;
use super::{Action, Context, Intent, Module, Reply, Skill, intent, output, pick, spawn};
use crate::grammar::{Slots, Value};
use std::env;
use std::time::{SystemTime, UNIX_EPOCH};
use tinyjson::JsonValue;

#[derive(Debug, Clone, PartialEq)]
pub enum Reminder {
    Set {
        what: String,
        seconds: u32,
    },
    /// Pending reminders, soonest first.
    List,
}

fn r(x: Reminder) -> Option<Action> {
    Some(Action::Reminder(x))
}

fn text(s: &Slots, name: &str) -> Option<String> {
    match s.get(name)? {
        Value::Text(t) => Some(t.clone()),
        _ => None,
    }
}

pub const MODULE: Module = Module {
    intents,
    slots: &[],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            &[
                "!remind me to {text} in {duration}",
                "!remind me in {duration} to {text}",
            ],
            |s, _| match s.get("duration")? {
                Value::Duration(seconds) => r(Reminder::Set {
                    what: text(s, "text")?,
                    seconds: *seconds,
                }),
                _ => None,
            },
        ),
        intent(
            &[
                "!(what|which) reminders (do|did) i have [set]",
                "!(what|which) reminders have i set",
                "!what are my [current] reminders",
                "!do i have any reminders",
                "![tell me] (what's|what is) on my (reminder|reminders) list [today]",
                "![can you] (read|tell|show|list|repeat) [me] [all] my [current] (reminders|reminder list|reminders list|list of reminders) [to me|back to me]",
                "![can you] tell me (what reminders|the reminders) i (have|i've) set [up]",
                "![can you] tell me what reminders i've set",
                "!read back [the|my] reminder list",
                "!what (did|do) i (want|need|ask you) to remember [again]",
                "!what did i ask to be reminded (about|of)",
            ],
            |_, _| r(Reminder::List),
        ),
    ]
}

impl Skill for Reminder {
    fn run(&self, _ctx: &Context) -> Reply {
        match self {
            Reminder::Set { what, seconds } => remind(what, *seconds),
            Reminder::List => reminders(),
        }
    }
}

fn exe() -> String {
    env::current_exe().map_or_else(|_| "voice-assistant".into(), |p| p.display().to_string())
}

fn remind(what: &str, seconds: u32) -> Reply {
    let when = format!("--on-active={seconds}s");
    let msg = format!("Reminder: {what}");
    let id = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| d.as_nanos());
    let unit = format!("--unit={REMINDER}{id}");
    let notify_unit = format!("--unit=va-notify-{id}");
    let description = format!("--description={msg}");
    // Transient user timers survive the assistant restarting.
    spawn(
        "systemd-run",
        &[
            "--user",
            "--collect",
            "--quiet",
            &notify_unit,
            &when,
            "notify-send",
            "-u",
            "critical",
            "Reminder",
            what,
        ],
    );
    spawn(
        "systemd-run",
        &[
            "--user",
            "--collect",
            "--quiet",
            &unit,
            &description,
            &when,
            &exe(),
            "--say",
            &msg,
        ],
    );
    let delay = spoken_delay(seconds);
    let speech = pick(
        &[
            "Okay, in {}, I'll nag you.",
            "Got it. {} from now, you'll hear from me.",
            "Fine. I'll remind you in {}. Don't make me repeat myself.",
            "In {}, I'll be in your ear. Like a conscience, but louder.",
        ],
        &delay,
    );
    Reply::live("Reminder", &format!("{what} in {delay}"), speech)
}

const REMINDER: &str = "va-reminder-";

fn spoken_delay(seconds: u32) -> String {
    let mins = seconds.div_ceil(60);
    let (h, m) = (mins / 60, mins % 60);
    let hours = match h {
        1 => "an hour".to_owned(),
        h => format!("{h} hours"),
    };
    match (seconds, h, m) {
        (s, _, _) if s < 60 => format!("{s} seconds"),
        (_, 0, 1) => "a minute".into(),
        (_, 0, m) => format!("{m} minutes"),
        (_, _, 0) => hours,
        (_, _, m) => format!("{hours} and {m} minutes"),
    }
}

/// Reminder timers from `systemctl list-timers --output=json`, with
/// seconds left. Elapsed ones have `next` 0.
fn pending(json: &str, now: u64) -> Vec<(String, u32)> {
    let Ok(JsonValue::Array(timers)) = json.parse::<JsonValue>() else {
        return Vec::new();
    };
    let mut out: Vec<(String, u32)> = timers
        .iter()
        .filter_map(|t| {
            let unit = json_text(key(t, "unit")?)?.to_owned();
            let JsonValue::Number(next) = key(t, "next")? else {
                return None;
            };
            #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
            let next = (*next / 1e6) as u64;
            let left = u32::try_from(next.checked_sub(now)?).ok()?;
            Some((unit, left))
        })
        .collect();
    out.sort_by_key(|(_, left)| *left);
    out
}

const NO_REMINDERS: &[&str] = &[
    "No reminders. Your memory is on its own.",
    "Nothing pending. Enjoy the freedom while it lasts.",
    "None. Either you're very organised or very forgetful.",
    "Zero. I have nothing to nag you about. I hate it.",
];
const REMINDERS: &[&str] = &[
    "{}.",
    "Here's what you made me remember: {}.",
    "{}. And no, I won't forget. Unlike some people.",
    "Coming up: {}. Don't say I didn't warn you.",
];

fn reminders() -> Reply {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| d.as_secs());
    let pattern = format!("{REMINDER}*");
    let json = output(
        "systemctl",
        &["--user", "list-timers", &pattern, "--output=json", "--all"],
    )
    .unwrap_or_default();
    let items: Vec<String> = pending(&json, now)
        .into_iter()
        .map(|(unit, left)| {
            let what = output(
                "systemctl",
                &["--user", "show", &unit, "-p", "Description", "--value"],
            )
            .unwrap_or_default();
            let what = what.strip_prefix("Reminder: ").unwrap_or(&what).to_owned();
            format!("{what} in {}", spoken_delay(left))
        })
        .collect();
    if items.is_empty() {
        return Reply::live("Reminders", "", pick(NO_REMINDERS, ""));
    }
    let what = spoken_list(&items);
    Reply::live("Reminders", &what, pick(REMINDERS, &what))
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    #[test]
    fn delays_in_hours() {
        assert_eq!(spoken_delay(30), "30 seconds");
        assert_eq!(spoken_delay(600), "10 minutes");
        assert_eq!(spoken_delay(3600), "an hour");
        assert_eq!(spoken_delay(5400), "an hour and 30 minutes");
        assert_eq!(spoken_delay(7200), "2 hours");
    }

    #[test]
    fn pending_reminders_soonest_first() {
        let json = r#"[{"next":1700000600000000,"unit":"va-reminder-2.timer"},
            {"next":1700000060000000,"unit":"va-reminder-1.timer"},
            {"next":0,"unit":"va-reminder-old.timer"}]"#;
        assert_eq!(
            pending(json, 1_700_000_000),
            [
                ("va-reminder-1.timer".to_owned(), 60),
                ("va-reminder-2.timer".to_owned(), 600)
            ]
        );
    }

    #[test]
    fn phrasings() {
        let p = |t| parse_at(t, &Context::at(0));
        for (text, want) in [
            (
                "Remind me to call mum in 10 minutes.",
                Reminder::Set {
                    what: "call mum".into(),
                    seconds: 600,
                },
            ),
            (
                "Remind me in an hour to check the oven",
                Reminder::Set {
                    what: "check the oven".into(),
                    seconds: 3600,
                },
            ),
            ("What reminders do I have?", Reminder::List),
            ("Tell me what's on my reminder list", Reminder::List),
            ("Read my reminders to me", Reminder::List),
            ("What did I want to remember again?", Reminder::List),
            ("Do I have any reminders?", Reminder::List),
        ] {
            assert_eq!(p(text), Some(Action::Reminder(want)), "{text:?}");
        }
    }
}
