//! Elliptic follow-ups resolved against the previous action:
//! "what's the weather?" ... "and tomorrow?", "tell me a joke" ... "another one".
//! Each skill decides what "again" and "and <x>" mean for its actions.

use super::{Action, Intent, intent, pick, slot};
use crate::clock::Clock;
use crate::grammar::Value;

const NOT_AGAIN: &[&str] = &[
    "Again? Once was plenty, darling.",
    "No encores for that one. I have standards.",
    "Do it again? What am I, your intern?",
    "Encore? Pay me first.",
];

const LOST: &[&str] = &[
    "Another what? I've lost the thread, babe.",
    "Again? We haven't even started.",
    "More of what? Use your words.",
    "Another one of what? Help me out here, I'm only software.",
];

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            &[
                "![tell|give|say|do] [me|us] (again|once more|one more|one more time|another|another one|and another|do it again|same again|more|even more|more please|keep going|a bit more|bit more)",
            ],
            |_, ctx| {
                let line = match &ctx.previous {
                    None => LOST,
                    Some(prev) if prev.skill().is_some_and(|s| s.repeatable()) => {
                        return Some(prev.clone());
                    }
                    Some(_) => NOT_AGAIN,
                };
                Some(Action::Say(pick(line, "")))
            },
        ),
        intent(
            &[
                "!(and|also|plus|what about|how about|and what about|and how about|same for|now|and now) {text}",
            ],
            |s, ctx| {
                let Value::Text(t) = s.get("text")? else {
                    return None;
                };
                let all: Vec<&str> = t.split_whitespace().collect();
                // "and in munich", "what about at 3"
                let words = match all.as_slice() {
                    ["in" | "at" | "for" | "on", rest @ ..] if !rest.is_empty() => rest,
                    w => w,
                };
                ctx.previous.as_ref()?.skill()?.refine(words)
            },
        ),
    ]
}

/// Helpers for `Skill::refine`.
pub fn day(words: &[&str]) -> Option<u32> {
    match slot("day", words)? {
        Value::Day(d) if d <= 2 => Some(d),
        _ => None,
    }
}

pub fn day_part(words: &[&str]) -> Option<(u32, usize)> {
    match slot("when", words)? {
        Value::DayPart(d, p) => Some((d, p)),
        _ => None,
    }
}

/// "at 3" for a meeting means 3 pm; meetings at 3 am are rare.
pub fn meeting_hours(c: &Clock) -> u32 {
    if c.twelve_hour && c.secs < 7 * 3600 {
        c.secs + 12 * 3600
    } else {
        c.secs
    }
}

pub fn afternoon_clock(words: &[&str]) -> Option<u32> {
    match slot("clock", words)? {
        Value::Clock(c) => Some(meeting_hours(&c)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::fun::Fun;
    use super::super::info::Info;
    use super::super::meetings::Meeting;
    use super::super::notes::Notes;
    use super::super::ops::Ops;
    use super::super::volume::Volume;
    use super::super::{Action, Context, parse_at};
    use crate::skills::timer::Timer;

    fn after(prev: Action, text: &str) -> Option<Action> {
        let ctx = Context {
            previous: Some(prev),
            ..Context::at(10 * 3600)
        };
        parse_at(text, &ctx)
    }

    fn weather(day: u32, place: Option<&str>, part: Option<usize>) -> Action {
        Action::Info(Info::Weather {
            day,
            place: place.map(Into::into),
            part,
        })
    }

    #[test]
    fn carries_over() {
        let w = weather(0, None, None);
        assert_eq!(
            after(w.clone(), "And tomorrow?"),
            Some(weather(1, None, None))
        );
        assert_eq!(
            after(w.clone(), "What about Munich?"),
            Some(weather(0, Some("munich"), None))
        );
        assert_eq!(
            after(w.clone(), "And in New York?"),
            Some(weather(0, Some("new york"), None))
        );
        assert_eq!(
            after(weather(0, Some("munich"), None), "And tomorrow?"),
            Some(weather(1, Some("munich"), None))
        );
        assert_eq!(
            after(w, "How about tonight?"),
            Some(weather(0, None, Some(7)))
        );

        let joke = Action::Fun(Fun::Joke);
        assert_eq!(after(joke.clone(), "Another one!"), Some(joke.clone()));
        assert_eq!(after(joke.clone(), "Again."), Some(joke.clone()));
        assert_eq!(after(joke.clone(), "tell another."), Some(joke));
        let up = Action::Volume(Volume::Up);
        assert_eq!(after(up.clone(), "More."), Some(up));

        assert_eq!(
            after(Action::Ops(Ops::HostUp(0)), "And eva?"),
            Some(Action::Ops(Ops::HostUp(1)))
        );
        assert_eq!(
            after(
                Action::Meeting(Meeting::Day { day: 0, part: None }),
                "And tomorrow?"
            ),
            Some(Action::Meeting(Meeting::Day { day: 1, part: None }))
        );
        assert_eq!(
            after(
                Action::Meeting(Meeting::FreeAt {
                    day: 0,
                    secs: 10 * 3600
                }),
                "What about at 3?"
            ),
            Some(Action::Meeting(Meeting::FreeAt {
                day: 0,
                secs: 15 * 3600
            }))
        );
        assert_eq!(
            after(
                Action::Notes(Notes::Add {
                    items: vec!["milk".into()],
                    list: 1
                }),
                "And eggs."
            ),
            Some(Action::Notes(Notes::Add {
                items: vec!["eggs".into()],
                list: 1
            }))
        );
    }

    #[test]
    fn cheeky_without_context() {
        assert_eq!(parse_at("And tomorrow?", &Context::at(0)), None);
        let say = |a: Option<Action>| matches!(a, Some(Action::Say(_)));
        assert!(say(parse_at("Another one", &Context::at(0))));
        // Cancelling twice is not a thing.
        assert!(say(after(Action::Timer(Timer::Cancel), "Again")));
    }
}
