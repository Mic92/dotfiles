//! Calendar and meetings: read via khal's JSON output, create via
//! calendar-cli (which syncs right away).

use super::json::{key, text};
use super::system;
use super::{
    Action, Context, Intent, Module, Reply, Skill, choose, followup, intent, output, pick, spawn,
};
use crate::clock;
use crate::grammar::{Slots, Value};
use tinyjson::JsonValue;

#[derive(Debug, Clone, PartialEq)]
pub enum Meeting {
    Next,
    Join,
    /// Agenda for a day (0 = today), optionally limited to a 3-hour slot.
    Day {
        day: u32,
        part: Option<usize>,
    },
    /// Free at a time of day (seconds since midnight) on a day?
    FreeAt {
        day: u32,
        secs: u32,
    },
    Create {
        title: String,
        day: u32,
        secs: u32,
        confirmed: bool,
    },
}

fn mt(x: Meeting) -> Option<Action> {
    Some(Action::Meeting(x))
}

fn day(s: &Slots) -> Option<u32> {
    match s.get("day") {
        Some(Value::Day(d)) => Some(*d),
        Some(_) => None,
        None => Some(0),
    }
}

/// "at 3" during the day means 3 pm; meetings at 3 am are rare.
fn clock_secs(s: &Slots) -> Option<u32> {
    match s.get("clock").or_else(|| s.get("time"))? {
        Value::Clock(c) => Some(followup::meeting_hours(c)),
        _ => None,
    }
}

fn title(s: &Slots, prefix: &str) -> Option<String> {
    match s.get("text")? {
        Value::Text(t) => Some(format!("{prefix}{t}")),
        _ => None,
    }
}

/// The greedy `{text}` slot may swallow an unmarked day ("with anna tomorrow").
fn split_day(title: &str) -> (&str, Option<u32>) {
    for (suffix, d) in [
        (" the day after tomorrow", 2),
        (" day after tomorrow", 2),
        (" tomorrow", 1),
        (" today", 0),
    ] {
        if let Some(t) = title.strip_suffix(suffix) {
            return (t, Some(d));
        }
    }
    (title, None)
}

fn create(s: &Slots, prefix: &str) -> Option<Action> {
    let full = title(s, prefix)?;
    let (t, d) = split_day(&full);
    let day = match (s.get("day"), d) {
        (None, Some(d)) => d,
        _ => day(s)?,
    };
    mt(Meeting::Create {
        title: t.to_owned(),
        day,
        secs: clock_secs(s)?,
        confirmed: false,
    })
}

pub const MODULE: Module = Module {
    intents,
    slots: &[],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            &[
                "!(what's|what is|when's|when is) my next (meeting|appointment|event|call)",
                "!(what's|what is) next [on my calendar]",
                "!(do i have|have i got|is there) a (meeting|call) (soon|now|coming up|right now)",
                "!how (long|much time) (until|till|before) my next (meeting|appointment|call)",
                "!(am i|are we) in a meeting [right now|now]",
            ],
            |_, _| mt(Meeting::Next),
        ),
        intent(
            &[
                "!(join|open|start|enter) [my|the|this] [next|current] (meeting|call|video call)",
                "!open [the] (meeting|meet|zoom|call) link",
                "!(take me to|let me into) [my|the] (meeting|call)",
            ],
            |_, _| mt(Meeting::Join),
        ),
        intent(
            &[
                "(what's|what is) on my (calendar|agenda|schedule) [{day}]",
                "what do i have [on] {day}",
                "(any|do i have) (meetings|appointments|events) {day}",
                "(any|do i have) (meetings|appointments|events)",
                "(how busy am i|how does my day look|what does my day look like) [{day}]",
                "!(calendar|agenda|schedule) [{day}]",
                "!what's my day like",
            ],
            |s, _| {
                mt(Meeting::Day {
                    day: day(s)?,
                    part: None,
                })
            },
        ),
        intent(
            &[
                "(what's|what is) on my (calendar|agenda|schedule) {when}",
                "what do i have {when}",
                "(any|do i have) (meetings|appointments|events) {when}",
                "!am i (free|busy) {when}",
            ],
            |s, _| match s.get("when")? {
                Value::DayPart(d, p) => mt(Meeting::Day {
                    day: *d,
                    part: Some(*p),
                }),
                _ => None,
            },
        ),
        intent(
            &[
                "!am i (free|busy|available) [{day}] at {clock}",
                "!am i (free|busy|available) at {clock} {day}",
                "!(do i have|have i got) (a meeting|anything|something|a call) [{day}] at {clock}",
                "!(do i have|have i got) (a meeting|anything|something|a call) at {clock} {day}",
                "!(is|are) [there] (anything|a meeting) at {clock} [{day}]",
            ],
            |s, _| {
                mt(Meeting::FreeAt {
                    day: day(s)?,
                    secs: clock_secs(s)?,
                })
            },
        ),
        intent(
            &[
                "!(schedule|create|add|book|set up|make|put in) [a|an] (meeting|call) with {text} [{day}] at {clock}",
                "!(schedule|create|add|book|set up|make|put in) [a|an] (meeting|call) with {text} at {clock} {day}",
            ],
            |s, _| create(s, "Meeting with "),
        ),
        intent(
            &[
                "!(schedule|create|add|book|set up|make|put in) [a|an] (meeting|appointment|event|call) (about|for|called|named) {text} [{day}] at {clock}",
                "!(schedule|create|add|book|set up|make|put in) [a|an] (meeting|appointment|event|call) (about|for|called|named) {text} at {clock} {day}",
                "!(schedule|create|add|book|put) {text} (in|on|into|to) [my] calendar [{day}] at {clock}",
                "!(schedule|create|add|book|put) {text} (in|on|into|to) [my] calendar at {clock} {day}",
            ],
            |s, _| create(s, ""),
        ),
    ]
}

#[derive(Debug, Clone, PartialEq)]
struct Event {
    /// "YYYY-MM-DD HH:MM", or "YYYY-MM-DD" for all-day events.
    start: String,
    end: String,
    title: String,
    link: Option<String>,
    all_day: bool,
}

const VIDEO: &[&str] = &[
    "meet.google.com",
    "zoom.us",
    "teams.microsoft.com",
    "teams.live.com",
    "meet.jit.si",
    "whereby.com",
    "webex.com",
];

fn video_link(fields: &[&str]) -> Option<String> {
    fields
        .iter()
        .flat_map(|f| f.split_whitespace())
        .find_map(|w| {
            let w = w.trim_matches(|c: char| matches!(c, '<' | '>' | '(' | ')' | '"' | ',' | ';'));
            (w.starts_with("https://") && VIDEO.iter().any(|v| w.contains(v))).then(|| w.to_owned())
        })
}

/// khal prints one JSON array per day.
fn parse_events(raw: &str) -> Vec<Event> {
    let field = |e: &JsonValue, k: &str| key(e, k).and_then(text).unwrap_or_default().to_owned();
    let mut out: Vec<Event> = raw
        .lines()
        .filter_map(|l| match l.parse::<JsonValue>() {
            Ok(JsonValue::Array(a)) => Some(a),
            _ => None,
        })
        .flatten()
        .filter_map(|e| {
            let title = field(&e, "title");
            let start = field(&e, "start-long");
            if title.is_empty() || start.is_empty() {
                return None;
            }
            let link = video_link(&[
                &field(&e, "location"),
                &field(&e, "url"),
                &field(&e, "description"),
            ]);
            Some(Event {
                start,
                end: field(&e, "end-long"),
                title,
                link,
                all_day: field(&e, "all-day") == "True",
            })
        })
        .collect();
    out.sort_by(|a, b| a.start.cmp(&b.start));
    out.dedup();
    out
}

fn khal(range: &[&str]) -> Option<Vec<Event>> {
    let mut args = vec!["list", "--day-format", ""];
    for f in [
        "start-long",
        "end-long",
        "title",
        "location",
        "url",
        "description",
        "all-day",
    ] {
        args.extend(["--json", f]);
    }
    args.extend_from_slice(range);
    output("khal", &args).map(|r| parse_events(&r))
}

/// Minutes since the epoch for "YYYY-MM-DD HH:MM" (or a bare date).
fn minutes(s: &str) -> Option<i64> {
    let (date, time) = s.trim().split_once(' ').unwrap_or((s.trim(), "00:00"));
    let mut d = date.splitn(3, '-').map(str::parse::<i64>);
    let (y, m, day) = (d.next()?.ok()?, d.next()?.ok()?, d.next()?.ok()?);
    let (h, min) = time.split_once(':')?;
    let (h, min) = (h.parse::<i64>().ok()?, min.parse::<i64>().ok()?);
    // Howard Hinnant's days_from_civil.
    let y = if m <= 2 { y - 1 } else { y };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let mp = (m + 9) % 12;
    let doy = (153 * mp + 2) / 5 + day - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    Some((era * 146_097 + doe - 719_468) * 1440 + h * 60 + min)
}

fn date_in(days: u32) -> Option<String> {
    output("date", &["-d", &format!("+{days} days"), "+%F"])
}

fn now() -> Option<String> {
    output("date", &["+%F %H:%M"])
}

fn spoken_mins(m: i64) -> String {
    match m {
        ..=0 => "now".into(),
        1 => "one minute".into(),
        2..=59 => format!("{m} minutes"),
        60..=119 => match m - 60 {
            0 => "an hour".into(),
            r => format!("an hour and {r} minutes"),
        },
        _ if m < 24 * 60 => format!("{} hours", (m + 30) / 60),
        _ => format!("{} days", (m + 720) / 1440),
    }
}

/// "14:00" -> "2:00 PM" using the shared formatter.
fn hm(start: &str) -> String {
    start
        .split_once(' ')
        .and_then(|(_, t)| t.split_once(':'))
        .and_then(|(h, m)| {
            Some(clock::format_hm(
                h.parse::<u32>().ok()? * 3600 + m.parse::<u32>().ok()? * 60,
            ))
        })
        .unwrap_or_default()
}

impl Skill for Meeting {
    fn refine(&self, words: &[&str]) -> Option<Action> {
        let new = match self {
            Meeting::Day { .. } => followup::day(words)
                .map(|day| Meeting::Day { day, part: None })
                .or_else(|| {
                    followup::day_part(words).map(|(day, p)| Meeting::Day { day, part: Some(p) })
                })?,
            Meeting::FreeAt { day, secs } => match followup::day(words) {
                Some(d) => Meeting::FreeAt {
                    day: d,
                    secs: *secs,
                },
                None => Meeting::FreeAt {
                    day: *day,
                    secs: followup::afternoon_clock(words)?,
                },
            },
            _ => return None,
        };
        mt(new)
    }

    fn run(&self, ctx: &Context) -> Reply {
        let x = self;
        match x {
            Meeting::Next => next(),
            Meeting::Join => join(),
            Meeting::Day { day, part } => agenda(*day, *part, ctx),
            Meeting::FreeAt { day, secs } => free_at(*day, *secs),
            Meeting::Create {
                title,
                day,
                secs,
                confirmed,
            } => create_event(title, *day, *secs, *confirmed),
        }
    }
}

const NO_CALENDAR: &[&str] = &[
    "I can't read your calendar. Khal is sulking.",
    "Your calendar is locked. Maybe it's hiding something from me.",
    "Calendar's not talking to me. We had a fight.",
];

/// The meeting in progress, or else the next one, with minutes until it.
fn current_or_next(events: &[Event], now: &str) -> Option<(Event, i64)> {
    let n = minutes(now)?;
    events
        .iter()
        .filter(|e| !e.all_day)
        .filter_map(|e| Some((e, minutes(&e.start)?, minutes(&e.end).unwrap_or(i64::MIN))))
        .find(|(_, start, end)| *start > n || *end > n)
        .map(|(e, start, _)| (e.clone(), start - n))
}

fn next() -> Reply {
    let (Some(events), Some(now)) = (khal(&["now", "7d"]), now()) else {
        return Reply::live("Calendar", "", pick(NO_CALENDAR, ""));
    };
    let Some((e, mins)) = current_or_next(&events, &now) else {
        return Reply::live(
            "Calendar",
            "",
            pick(
                &[
                    "Nothing in the next week. Are you on holiday, or just unpopular?",
                    "No meetings ahead. Bliss.",
                    "Nothing scheduled. Did everyone finally give up on you?",
                ],
                "",
            ),
        );
    };
    let when = if mins > 12 * 60 {
        format!("{} at {}", &e.start[..10], hm(&e.start))
    } else {
        hm(&e.start)
    };
    let what = format!("{} at {when}", e.title);
    let speech = match mins {
        ..=0 => pick(
            &[
                "You're in {} right now! Why are you talking to me?",
                "{}. That's now. Go, go, go!",
            ],
            &e.title,
        ),
        1..=5 => format!("{} in {}! Hurry up, gorgeous.", e.title, spoken_mins(mins)),
        6..=60 => format!(
            "{} in {}. Time for a coffee first.",
            e.title,
            spoken_mins(mins)
        ),
        _ => format!("Next up: {what}. That's in {}. Relax.", spoken_mins(mins)),
    };
    let speech = if e.link.is_some() && mins <= 10 {
        format!("{speech} Say join my meeting.")
    } else {
        speech
    };
    Reply::live("Next meeting", &what, speech)
}

fn join() -> Reply {
    let (Some(events), Some(now)) = (khal(&["now", "1d"]), now()) else {
        return Reply::live("Calendar", "", pick(NO_CALENDAR, ""));
    };
    match current_or_next(&events, &now) {
        Some((e, mins)) if mins <= 15 => match &e.link {
            Some(link) => {
                spawn("xdg-open", &[link]);
                let speech = if mins > 2 {
                    format!(
                        "Opening {}. You're {} early. Teacher's pet.",
                        e.title,
                        spoken_mins(mins)
                    )
                } else {
                    pick(
                        &[
                            "Opening {}. Fix your hair.",
                            "Joining {}. Camera off if you're in pyjamas.",
                            "Joining {}. Mute yourself, I've heard you chew.",
                            "{}. Go get them.",
                        ],
                        &e.title,
                    )
                };
                Reply::live("Join meeting", &e.title, speech)
            }
            None => Reply::live(
                "Join meeting",
                &e.title,
                format!("{} has no video link. In person? How retro.", e.title),
            ),
        },
        Some((e, mins)) => Reply::live(
            "Join meeting",
            &e.title,
            format!(
                "Your next meeting, {}, is in {}. Too early. Nobody likes the early bird.",
                e.title,
                spoken_mins(mins)
            ),
        ),
        None => Reply::live(
            "Join meeting",
            "",
            pick(
                &[
                    "There's no meeting to join. Free as a bird.",
                    "No meeting. Nobody wants to see your face today. Lucky you.",
                ],
                "",
            ),
        ),
    }
}

fn day_name(day: u32) -> &'static str {
    match day {
        0 => "today",
        1 => "tomorrow",
        _ => "the day after tomorrow",
    }
}

fn agenda(day: u32, part: Option<usize>, ctx: &Context) -> Reply {
    let Some(date) = date_in(day) else {
        return Reply::live("Calendar", "", pick(NO_CALENDAR, ""));
    };
    let Some(mut events) = khal(&[&date, "1d"]) else {
        return Reply::live("Calendar", "", pick(NO_CALENDAR, ""));
    };
    events.retain(|e| e.start.starts_with(&date) || e.all_day);
    if let Some(p) = part {
        // 3-hour forecast slots; a meeting counts if it starts within the slot.
        let (from, to) = (p * 3 * 60, (p + 1) * 3 * 60 + if p == 7 { 60 } else { 0 });
        events.retain(|e| {
            let m = minutes(&e.start).zip(minutes(&date)).map(|(s, d)| s - d);
            m.is_some_and(|m| usize::try_from(m).is_ok_and(|m| (from..to).contains(&m)))
        });
    }
    let when = day_name(day);
    let body = events
        .iter()
        .map(|e| {
            if e.all_day {
                format!("all day {}", e.title)
            } else {
                format!("{} {}", hm(&e.start), e.title)
            }
        })
        .collect::<Vec<_>>()
        .join("\n");
    let timed: Vec<&Event> = events.iter().filter(|e| !e.all_day).collect();
    let speech = match timed.as_slice() {
        [] if part.is_some() => pick(
            &[
                "You're free then. Nap opportunity!",
                "Nothing then. Guard that time with your life.",
                "Free. Block it before someone books it.",
            ],
            "",
        ),
        [] => choose(
            &[(
                ctx.weekday == 5 && day == 0,
                &["Nothing {} and it's Friday. Leave early, I won't tell."][..],
            )],
            &[
                "Nothing on your calendar {}. Free as a bird.",
                "Your calendar is empty {}. Netflix?",
                "Nothing {}. Pretend to be busy anyway.",
            ],
            when,
        ),
        [e] => format!("Just one thing {when}: {} at {}.", e.title, hm(&e.start)),
        [a, b] => format!(
            "Two things {when}: {} at {}, and {} at {}.",
            a.title,
            hm(&a.start),
            b.title,
            hm(&b.start)
        ),
        [first, .., last] if timed.len() >= 6 => format!(
            "{} meetings {when}. Starting with {} at {}, ending with {} at {}. Who hurt you?",
            timed.len(),
            first.title,
            hm(&first.start),
            last.title,
            hm(&last.start)
        ),
        [first, ..] => format!(
            "You have {} things {when}. First up: {} at {}. Busy, busy.",
            timed.len(),
            first.title,
            hm(&first.start)
        ),
    };
    Reply::live(&format!("Calendar {when}"), &body, speech)
}

fn free_at(day: u32, secs: u32) -> Reply {
    let Some(date) = date_in(day) else {
        return Reply::live("Calendar", "", pick(NO_CALENDAR, ""));
    };
    let Some(events) = khal(&[&date, "1d"]) else {
        return Reply::live("Calendar", "", pick(NO_CALENDAR, ""));
    };
    let t = minutes(&date).map(|d| d + i64::from(secs / 60));
    let clash = t.and_then(|t| {
        events.iter().filter(|e| !e.all_day).find(|e| {
            let (s, end) = (minutes(&e.start), minutes(&e.end));
            s.is_some_and(|s| s <= t) && end.is_some_and(|end| end > t)
        })
    });
    let when = format!("{} {}", clock::format_hm(secs), day_name(day));
    match clash {
        Some(e) => Reply::live(
            "Calendar",
            &e.title,
            format!("No, at {when} you have {}. Popular, aren't you?", e.title),
        ),
        None => Reply::live(
            "Calendar",
            &when,
            pick(
                &[
                    "You're free at {}. Want me to book it?",
                    "{} is free. Treasure it.",
                ],
                &when,
            ),
        ),
    }
}

fn timezone() -> String {
    output("timedatectl", &["show", "--property=Timezone", "--value"])
        .unwrap_or_else(|| "Europe/Berlin".into())
}

fn create_event(title: &str, day: u32, secs: u32, confirmed: bool) -> Reply {
    let when = format!("{} {}", day_name(day), clock::format_hm(secs));
    let summary = capitalize(title);
    if !confirmed {
        let mut r = Reply::live(
            "New meeting?",
            &format!("{summary}, {when}"),
            format!("{summary}, {when}. Shall I add it?"),
        );
        let (title, title2) = (title.to_owned(), summary.clone());
        r.follow_up = Some(Box::new(move |text, _| match system::yes(text)? {
            true => mt(Meeting::Create {
                title,
                day,
                secs,
                confirmed: true,
            }),
            false => Some(Action::Say(format!(
                "Okay, no {title2}. Your calendar thanks you."
            ))),
        }));
        return r;
    }
    let Some(date) = date_in(day) else {
        return Reply::live("Calendar", "", pick(NO_CALENDAR, ""));
    };
    let start = format!("{date} {:02}:{:02}", secs / 3600, secs % 3600 / 60);
    let tz = timezone();
    match output(
        "calendar-cli",
        &[
            "new",
            &summary,
            "--start",
            &start,
            "--timezone",
            &tz,
            "-d",
            "60",
            "--alarm",
            "10m",
        ],
    ) {
        Some(_) => Reply::live(
            "Meeting added",
            &format!("{summary}, {when}"),
            pick(
                &[
                    "Done. {} is in your calendar.",
                    "Booked: {}. I'll remind you ten minutes before.",
                    "{}, booked. Try to show up on time for once.",
                    "{}. Added. Look at you, organised.",
                ],
                &summary,
            ),
        ),
        None => Reply::live(
            "Calendar",
            &summary,
            "Calendar CLI refused. Try again, or do it by hand like a caveman.".into(),
        ),
    }
}

fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    c.next()
        .map(|f| f.to_uppercase().chain(c).collect())
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    #[test]
    fn phrasings() {
        let p = |t| parse_at(t, &Context::at(10 * 3600));
        for (text, want) in [
            ("When is my next meeting?", Meeting::Next),
            ("What's next on my calendar?", Meeting::Next),
            ("How long until my next meeting?", Meeting::Next),
            ("Join my meeting.", Meeting::Join),
            ("Open the zoom link", Meeting::Join),
            ("Start the call", Meeting::Join),
            (
                "What's on my calendar today?",
                Meeting::Day { day: 0, part: None },
            ),
            (
                "Do I have any meetings today?",
                Meeting::Day { day: 0, part: None },
            ),
            (
                "What do I have tomorrow?",
                Meeting::Day { day: 1, part: None },
            ),
            (
                "How busy am I tomorrow?",
                Meeting::Day { day: 1, part: None },
            ),
            ("Agenda", Meeting::Day { day: 0, part: None }),
            (
                "Am I free this afternoon?",
                Meeting::Day {
                    day: 0,
                    part: Some(5),
                },
            ),
            (
                "What do I have tomorrow morning?",
                Meeting::Day {
                    day: 1,
                    part: Some(3),
                },
            ),
            (
                "Am I free at 3?",
                Meeting::FreeAt {
                    day: 0,
                    secs: 15 * 3600,
                },
            ),
            (
                "Am I free tomorrow at 10 am?",
                Meeting::FreeAt {
                    day: 1,
                    secs: 10 * 3600,
                },
            ),
            (
                "Do I have a meeting at 4 pm?",
                Meeting::FreeAt {
                    day: 0,
                    secs: 16 * 3600,
                },
            ),
            (
                "Schedule a meeting with Anna tomorrow at 3 pm.",
                Meeting::Create {
                    title: "Meeting with anna".into(),
                    day: 1,
                    secs: 15 * 3600,
                    confirmed: false,
                },
            ),
            (
                "Add dentist to my calendar tomorrow at 9 am",
                Meeting::Create {
                    title: "dentist".into(),
                    day: 1,
                    secs: 9 * 3600,
                    confirmed: false,
                },
            ),
            (
                "Book a call about the budget at 2",
                Meeting::Create {
                    title: "the budget".into(),
                    day: 0,
                    secs: 14 * 3600,
                    confirmed: false,
                },
            ),
        ] {
            assert_eq!(p(text), Some(Action::Meeting(want)), "{text:?}");
        }
    }

    const KHAL: &str = r#"[{"start-long": "2026-10-23 14:00", "end-long": "2026-10-23 14:40", "title": "Anthropic sync", "location": "https://meet.google.com/xqm-fcdh-vmi", "url": "", "description": "", "all-day": "False"}]
[]
[{"start-long": "2026-10-24", "end-long": "2026-10-25", "title": "Holiday", "location": "", "url": "", "description": "", "all-day": "True"}, {"start-long": "2026-10-24 09:00", "end-long": "2026-10-24 09:30", "title": "Standup", "location": "Room 4", "url": "", "description": "Join: <https://zoom.us/j/123>", "all-day": "False"}]"#;

    #[test]
    fn events() {
        let ev = parse_events(KHAL);
        assert_eq!(ev.len(), 3);
        assert_eq!(
            ev[0].link.as_deref(),
            Some("https://meet.google.com/xqm-fcdh-vmi")
        );
        assert!(ev[1].all_day);
        assert_eq!(ev[2].link.as_deref(), Some("https://zoom.us/j/123"));
        let (e, mins) = current_or_next(&ev, "2026-10-23 13:35").expect("next");
        assert_eq!((e.title.as_str(), mins), ("Anthropic sync", 25));
        let (e, mins) = current_or_next(&ev, "2026-10-23 14:10").expect("ongoing");
        assert_eq!((e.title.as_str(), mins), ("Anthropic sync", -10));
        let (e, _) = current_or_next(&ev, "2026-10-23 15:00").expect("tomorrow");
        assert_eq!(e.title, "Standup");
        assert!(current_or_next(&ev, "2026-10-25 00:00").is_none());
    }

    #[test]
    fn time_maths() {
        assert_eq!(minutes("1970-01-01 00:00"), Some(0));
        assert_eq!(minutes("1970-01-02 01:30"), Some(1440 + 90));
        assert_eq!(
            minutes("2026-03-01 00:00")
                .zip(minutes("2026-02-28 00:00"))
                .map(|(a, b)| a - b),
            Some(1440)
        );
        assert_eq!(
            minutes("2024-03-01")
                .zip(minutes("2024-02-28"))
                .map(|(a, b)| a - b),
            Some(2880)
        );
        assert_eq!(spoken_mins(25), "25 minutes");
        assert_eq!(spoken_mins(75), "an hour and 15 minutes");
        assert_eq!(hm("2026-10-23 14:00"), "2 PM");
        assert_eq!(hm("2026-10-23 14:30"), "2:30 PM");
    }
}
