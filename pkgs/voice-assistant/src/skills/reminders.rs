//! Spoken reminders as transient systemd user timers, so they survive
//! the assistant restarting.

use super::dialogue::Question;
use super::json::{key, text as json_text};
use super::notes::spoken_list;
use super::{Action, Context, Intent, Module, Reply, Skill, intent, output, pick, slot, spawn};
use crate::clock;
use crate::grammar::{Slots, Value, normalize};
use std::env;
use std::time::{SystemTime, UNIX_EPOCH};
use tinyjson::JsonValue;

#[derive(Debug, Clone, PartialEq)]
pub enum Reminder {
    Set {
        what: String,
        seconds: u32,
    },
    /// A reminder missing its text or time: ask for it.
    Ask {
        what: Option<String>,
        /// Days ahead, when only the day was said.
        day: Option<u32>,
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

/// "remind me to", "set a reminder for me to".
const LEADS: &[&str] = &[
    "![can you|could you|please] remind me (to|about|that)",
    "![can you|could you|please] (set|make|create|add|set up) [me] [a] reminder [for me] (to|about|that|for)",
    "!(i need|i want) [a] reminder (to|about|that|for)",
    "!(i need|i want|i would like|i'd like) to (set|make|set up) [a] [new] reminder [for myself] (to|about|that|for)",
    "!(i need|i want) to (be|get) reminded (to|about|of)",
    "![can you] give me a reminder (to|about|for)",
];

/// Reminders with their time at the end ("in 10 minutes") or right after
/// "remind me" ("remind me in 10 minutes to").
fn timed(when: &str, word: &str) -> Vec<String> {
    let at = if word.is_empty() {
        when.to_owned()
    } else {
        format!("{word} {when}")
    };
    let mut v: Vec<String> = LEADS.iter().map(|l| format!("{l} {{text}} {at}")).collect();
    v.push(format!(
        "![can you|could you|please] remind me {at} (to|about|that) {{text}}"
    ));
    v
}

fn strs(v: &[String]) -> Vec<&str> {
    v.iter().map(String::as_str).collect()
}

fn with_text() -> Vec<String> {
    LEADS.iter().map(|l| format!("{l} {{text}}")).collect()
}

fn day(s: &Slots) -> Option<u32> {
    match s.get("day")? {
        Value::Day(d) => Some(*d),
        _ => None,
    }
}

fn set_in(s: &Slots, ctx: &Context) -> Option<Action> {
    let what = text(s, "text")?;
    let seconds = match (s.get("duration"), s.get("clock")) {
        (Some(Value::Duration(d)), _) => *d,
        (_, Some(Value::Clock(c))) => match day(s) {
            Some(d) => c.seconds_on(ctx.now, d)?,
            None => c.seconds_from(ctx.now),
        },
        _ => return None,
    };
    r(Reminder::Set { what, seconds })
}

pub const MODULE: Module = Module {
    intents,
    slots: &[],
};

pub fn intents() -> Vec<Intent> {
    vec![
        // A time first: open text would swallow "call mum in 10 minutes".
        intent(&strs(&timed("{duration}", "in")), set_in),
        intent(&strs(&timed("{day} at {clock}", "")), set_in),
        intent(&strs(&timed("at {clock} {day}", "")), set_in),
        intent(&strs(&timed("{clock}", "at")), set_in),
        intent(&strs(&timed("{day}", "")), |s, _| {
            r(Reminder::Ask {
                what: text(s, "text"),
                day: day(s),
            })
        }),
        intent(&strs(&with_text()), |s, _| {
            r(Reminder::Ask {
                what: text(s, "text"),
                day: None,
            })
        }),
        intent(
            &[
                "![can you|could you|will you|please] (set|make|create|add|give|set up) [me] [a] reminder [for me] [please]",
                "!(i need|i want|i would like|i'd like) [to set|to make] [a] reminder [please]",
                "![please] remind me [later] [please]",
                "![please] set [a] new reminder",
                "!(i need|i want) to (set|set up|make) [a] reminder [for myself]",
                "!can i set a reminder",
            ],
            |_, _| {
                r(Reminder::Ask {
                    what: None,
                    day: None,
                })
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
            Reminder::Ask { what: None, day } => {
                let mut r = Reply::new("Reminder", "", pick(ASK_WHAT, ""));
                r.question = Some(Box::new(AskWhat(*day)));
                r
            }
            Reminder::Ask {
                what: Some(what),
                day,
            } => {
                let lines = if *day == Some(1) {
                    ASK_TOMORROW
                } else {
                    ASK_WHEN
                };
                let mut r = Reply::new("Reminder", what, pick(lines, what));
                r.question = Some(Box::new(AskWhen {
                    what: what.clone(),
                    day: *day,
                }));
                r
            }
            Reminder::List => reminders(),
        }
    }
}

const ASK_WHAT: &[&str] = &[
    "Remind you of what?",
    "Sure. What shall I nag you about?",
    "What do you want me to remember? Since you won't.",
    "Go on, what is it?",
];
const ASK_WHEN: &[&str] = &[
    "When should I remind you to {}?",
    "{}. And when?",
    "When? Give me a time or I'll pick a terrible one.",
    "When do you want to be nagged?",
];
const ASK_TOMORROW: &[&str] = &[
    "Tomorrow at what time?",
    "Tomorrow when? Before or after your Club-Mate?",
];

/// "What shall I remind you of?"
struct AskWhat(Option<u32>);

impl Question for AskWhat {
    fn answer(&self, text: &str, _: &Context) -> Option<Action> {
        let owned = normalize(text);
        let mut words: Vec<&str> = owned.iter().map(String::as_str).collect();
        if let ["to" | "about" | "that" | "of", rest @ ..] = words.as_slice() {
            words = rest.to_vec();
        }
        (!words.is_empty()).then(|| {
            Action::Reminder(Reminder::Ask {
                what: Some(words.join(" ")),
                day: self.0,
            })
        })
    }
}

/// "When should I remind you?": "in 20 minutes", "at 5", "tomorrow at 9".
struct AskWhen {
    what: String,
    day: Option<u32>,
}

impl Question for AskWhen {
    fn answer(&self, text: &str, ctx: &Context) -> Option<Action> {
        let owned = normalize(text);
        let mut words: Vec<&str> = owned
            .iter()
            .map(String::as_str)
            .filter(|w| !matches!(*w, "um" | "uh" | "please" | "alexa"))
            .collect();
        let mut day = self.day;
        for n in [3, 2, 1] {
            if words.len() >= n
                && let Some(Value::Day(d)) = slot("day", &words[..n])
            {
                day = Some(d);
                words.drain(..n);
                break;
            }
        }
        if let ["in" | "at" | "for", rest @ ..] = words.as_slice() {
            words = rest.to_vec();
        }
        let seconds = match slot("duration", &words) {
            Some(Value::Duration(d)) if day.is_none() => d,
            _ => {
                let c = clock::parse(&words)?;
                match day {
                    Some(d) => c.seconds_on(ctx.now, d)?,
                    None => c.seconds_from(ctx.now),
                }
            }
        };
        Some(Action::Reminder(Reminder::Set {
            what: self.what.clone(),
            seconds,
        }))
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
    use super::super::dialogue::respond;
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

    fn ask(what: Option<&str>, day: Option<u32>) -> Reminder {
        Reminder::Ask {
            what: what.map(Into::into),
            day,
        }
    }

    fn set(what: &str, seconds: u32) -> Reminder {
        Reminder::Set {
            what: what.into(),
            seconds,
        }
    }

    #[test]
    fn missing_parts_are_asked_for() {
        let ctx = Context::at(14 * 3600);
        let p = |t| parse_at(t, &ctx);
        for (text, want) in [
            ("remind me to call my mom", ask(Some("call my mom"), None)),
            (
                "set a reminder to feed the dogs",
                ask(Some("feed the dogs"), None),
            ),
            (
                "make a reminder for me to do my taxes",
                ask(Some("do my taxes"), None),
            ),
            (
                "remind me to call the vet tomorrow",
                ask(Some("call the vet"), Some(1)),
            ),
            (
                "remind me tomorrow to call my mother",
                ask(Some("call my mother"), Some(1)),
            ),
            ("set a reminder", ask(None, None)),
            ("can you set me a reminder", ask(None, None)),
            ("remind me", ask(None, None)),
            (
                "i need to set a reminder to go to the post office",
                ask(Some("go to the post office"), None),
            ),
            (
                "i want to get reminded to clean my room",
                ask(Some("clean my room"), None),
            ),
            ("remind me to call mum at 5 pm", set("call mum", 3 * 3600)),
            (
                "remind me to call mum tomorrow at 9",
                set("call mum", 19 * 3600),
            ),
            (
                "set a reminder to call mum in 10 minutes",
                set("call mum", 600),
            ),
        ] {
            assert_eq!(p(text), Some(Action::Reminder(want)), "{text:?}");
        }
        // Alarms stay alarms.
        assert!(matches!(p("remind me at 5 pm"), Some(Action::Timer(_))));
    }

    #[test]
    fn questions_fill_the_gaps() {
        let ctx = Context::at(14 * 3600);
        let answer = |r: Reminder, t| {
            let q = r.run(&ctx).question.expect("asks");
            respond(t, &ctx, Some(q.as_ref()))
        };
        let call = || ask(Some("call mum"), None);
        for (t, secs) in [
            ("in 20 minutes", 1200),
            ("20 minutes", 1200),
            ("at 5 pm", 3 * 3600),
            ("tomorrow at 9", 19 * 3600),
        ] {
            assert_eq!(
                answer(call(), t),
                Some(Action::Reminder(set("call mum", secs))),
                "{t:?}"
            );
        }
        assert_eq!(
            answer(ask(Some("call mum"), Some(1)), "9 am"),
            Some(Action::Reminder(set("call mum", 19 * 3600)))
        );
        assert_eq!(
            answer(ask(None, None), "to water the plants"),
            Some(Action::Reminder(ask(Some("water the plants"), None)))
        );
        assert_eq!(answer(ask(None, None), "never mind"), Some(Action::Dismiss));
        assert_eq!(answer(call(), "forget it"), Some(Action::Dismiss));
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
