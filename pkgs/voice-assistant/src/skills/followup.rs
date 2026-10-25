//! Helpers for `Skill::refine`: "what's the weather?" ... "and tomorrow?".
//! `dialogue` decides when a refinement is meant.

use super::slot;
use crate::clock::Clock;
use crate::grammar::Value;

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
        let mut ctx = Context::at(10 * 3600);
        ctx.focus.previous = Some(prev);
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
