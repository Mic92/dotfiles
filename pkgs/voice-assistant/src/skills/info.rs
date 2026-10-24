//! Questions: time, date, weather, battery, uptime, calendar, mail, maths,
//! clipboard.

use super::json::{first_value, idx, key, num};
use super::{
    Action, Context, Intent, Module, Reply, Skill, choose, followup, intent, output, pick,
};
use crate::clock;
use crate::duration::number_words;
use crate::grammar::Value;
use crate::http;
use std::fs;
use std::slice;
use tinyjson::JsonValue;

#[derive(Debug, Clone, PartialEq)]
pub enum Info {
    Time,
    Date,
    /// Days from today (0..=2), and a place or here.
    Weather {
        day: u32,
        place: Option<String>,
        /// Index into the 3-hourly forecast; the whole day when unset.
        part: Option<usize>,
    },
    Battery,
    Uptime,
    Mail,
    Math(f64),
    Clipboard,
}

fn i(x: Info) -> Option<Action> {
    Some(Action::Info(x))
}

pub const MODULE: Module = Module {
    intents,
    slots: &[
        ("expr", |w, _| calculate(w).map(Value::Real)),
        ("place", |w, _| place(w).map(Value::Text)),
    ],
    vocab: &[],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            Some("What time is it?"),
            &[
                "!what time is it",
                "!what's the time",
                "!now's the time",
                "!what is the time",
                "!(tell|give) me the time",
                "!what time is it [now|right now]",
                "!(do you know|have you got) the time",
                "!how late is it",
                "!what's the current time",
                "!what is the current time",
                "!current time",
                "!time [please|now]",
                "!what hour is it",
                "!what o'clock is it",
            ],
            |_, _| i(Info::Time),
        ),
        intent(
            None,
            &[
                "!what's [the] date [today]",
                "!what is [the] date [today]",
                "!what day is [it] [today]",
                "!what's today's date",
                "!what's today",
            ],
            |_, _| i(Info::Date),
        ),
        intent(
            Some("What's the weather like?"),
            &[
                "what's the weather [like] [today|outside|now]",
                "what is the weather [like] [today|outside|now]",
                "how's the weather [today|outside]",
                "how is the weather [today|outside]",
                "(is|will) it (rain|raining|snow|snowing|cold|hot|warm|sunny) [today|outside]",
                "do i need (an|a) (umbrella|jacket|coat)",
                "!weather [today|now]",
                "!temperature [outside]",
                // Forecast and elsewhere.
                "(what's|what is|how's|how is|what will be) the weather [like] [be] {day}",
                "(what's|what is|how's|how is) the weather [like] (in|at) {place}",
                "(what's|what is|how's|how is|what will be) the weather [like] [be] (in|at) {place} {day}",
                "(what's|what is|how's|how is|what will be) the weather [like] [be] {day} (in|at) {place}",
                "(is|will) it (rain|snow|be cold|be hot|be warm|be sunny) {day}",
                "(is|will) it (rain|snow|be cold|be hot|be warm|be sunny) (in|at) {place} [{day}]",
                "do i need (an|a) (umbrella|jacket|coat) {day}",
                "!weather {day}",
                "!weather (in|at|for) {place}",
                "!(weather|forecast) (in|at|for) {place} {day}",
                "!forecast [for] [{day}]",
                "what will the weather be [like] {day}",
                "what will the weather be [like] (in|at) {place}",
                "what will the weather be [like] {day} (in|at) {place}",
                "what will the weather be [like] (in|at) {place} {day}",
                "(what's|what is|how's|how is|what will be) the weather [like] [be] {when}",
                "what will the weather be [like] {when}",
                "(what's|what is|how's|how is|what will be) the weather [like] [be] (in|at) {place} {when}",
                "(what's|what is|how's|how is|what will be) the weather [like] [be] {when} (in|at) {place}",
                "(is|will) it (rain|snow|be cold|be hot|be warm|be sunny) {when}",
                "do i need (an|a) (umbrella|jacket|coat) {when}",
                "!weather {when}",
            ],
            |s, _| {
                let (day, part) = match s.get("day").or_else(|| s.get("when")) {
                    Some(Value::Day(d)) if *d <= 2 => (*d, None),
                    Some(Value::DayPart(d, p)) if *d <= 2 => (*d, Some(*p)),
                    Some(_) => return None,
                    None => (0, None),
                };
                let place = match s.get("place") {
                    Some(Value::Text(p)) => Some(p.clone()),
                    _ => None,
                };
                i(Info::Weather { day, place, part })
            },
        ),
        intent(
            None,
            &[
                "how much battery [do i have] [left]",
                "[what's|what|how's|how] [is] [the|my] battery [level|status|charge] [at]",
                "!battery",
            ],
            |_, _| i(Info::Battery),
        ),
        intent(
            None,
            &[
                "how long [has] [the|my|this] (computer|laptop|system) been (on|up|running)",
                "!uptime",
                "how long have you been (on|up|running|awake)",
            ],
            |_, _| i(Info::Uptime),
        ),
        intent(
            None,
            &[
                "(do|have) i (have|got) [any] [new|unread] (mail|email|emails|mails|messages)",
                "(any|how many) [new|unread] (mail|email|emails|mails)",
                "check [my] (mail|email|emails|inbox)",
            ],
            |_, _| i(Info::Mail),
        ),
        intent(
            None,
            &["!(what's|what|calculate|compute|how) [is|much|many] [is] [the] {expr}"],
            |s, _| match s.get("expr")? {
                Value::Real(v) => i(Info::Math(*v)),
                _ => None,
            },
        ),
        intent(
            None,
            &[
                "(read|say) [me] [the|my|what's] [in] [the|my] clipboard",
                "what's in [the|my] clipboard",
                "what is in [the|my] clipboard",
            ],
            |_, _| i(Info::Clipboard),
        ),
    ]
}

/// "5 plus 3", "seven times six", "square root of 81", "20 percent of 50".
pub fn calculate(words: &[&str]) -> Option<f64> {
    if let ["square", "root", "of", rest @ ..] = words {
        return number_words(rest).filter(|v| *v >= 0.0).map(f64::sqrt);
    }
    let op_at = words.iter().position(|w| {
        matches!(
            *w,
            "plus"
                | "minus"
                | "times"
                | "multiplied"
                | "divided"
                | "over"
                | "percent"
                | "x"
                | "squared"
                | "cubed"
        )
    })?;
    let a = number_words(&words[..op_at])?;
    let rest = &words[op_at + 1..];
    let rest = rest
        .strip_prefix(&["by"])
        .or_else(|| rest.strip_prefix(&["of"]))
        .unwrap_or(rest);
    let v = match words[op_at] {
        "squared" if rest.is_empty() => a * a,
        "cubed" if rest.is_empty() => a * a * a,
        op => {
            let b = number_words(rest)?;
            match op {
                "plus" => a + b,
                "minus" => a - b,
                "times" | "multiplied" | "x" => a * b,
                "percent" => a / 100.0 * b,
                _ if b == 0.0 => f64::INFINITY,
                _ => a / b,
            }
        }
    };
    Some(v)
}

fn spoken_number(v: f64) -> String {
    if v.is_infinite() {
        return "infinity".into();
    }
    let rounded = (v * 100.0).round() / 100.0;
    if rounded.fract() == 0.0 {
        format!("{rounded:.0}")
    } else {
        format!("{rounded}")
    }
}

const TIME: &[&str] = &[
    "It's {}. Time flies when you're talking to me.",
    "{}. You're welcome.",
    "It's {}. Shouldn't you be doing something?",
    "{}, gorgeous.",
    "{}. And yes, you're late.",
    "{}. Time to stop asking me and start doing.",
];
const TIME_LATE: &[&str] = &[
    "It's {}. Why are you still up?",
    "{}. Go to bed, you gremlin.",
    "{}. Nothing good happens on a laptop at this hour.",
];
const TIME_EARLY: &[&str] = &[
    "{}. Way too early. Coffee first.",
    "It's {}. Ugh, mornings.",
    "{}. Even the coffee machine is asleep.",
];
const TIME_LUNCH: &[&str] = &[
    "It's {}. Lunch time! Feed yourself.",
    "{}. Your stomach already knew.",
    "{}. Step away from the keyboard and chew something.",
];
const TIME_EVENING: &[&str] = &[
    "It's {}. Wine o'clock, almost.",
    "{}. Close the laptop, maybe?",
    "{}. Sofa time. You've earned it, maybe.",
];
const DATE: &[&str] = &[
    "It's {}.",
    "Today is {}. Another day, another dollar.",
    "{}. Don't forget to call your mother.",
    "{}. Mark it, I might get you something.",
];
const MONDAY: &[&str] = &[
    "{}. Yes, it's Monday. I'm sorry.",
    "Ugh. {}. Mondays should be illegal.",
    "{}. Monday again. Who keeps scheduling these?",
];
const FRIDAY: &[&str] = &[
    "{}! It's Friday, baby!",
    "{}. Weekend is so close I can taste it.",
    "{}. Friday! No deploys, darling. I mean it.",
];
const WEEKEND: &[&str] = &[
    "{}. Weekend! Why are you talking to me?",
    "{}. No work today. Hopefully.",
    "{}. Weekend. Step away from the terminal.",
];
const WEATHER: &[&str] = &[
    "{}.",
    "Outside: {}. Inside: me, looking good.",
    "{}. Now you know.",
    "{}. Dress accordingly. Or don't, I can't see you.",
    "{}. Perfect excuse to stay in.",
];
const RAINY: &[&str] = &[
    "{}. Take an umbrella, sweetie.",
    "{}. Perfect weather for staying in with me.",
    "{}. Wet. Like your git workflow.",
];
const COLD: &[&str] = &[
    "{}. Brr. Wear a jacket, I mean it.",
    "{}. Cold. Cuddle weather.",
    "{}. Bundle up, buttercup.",
];
const HOT: &[&str] = &["{}. Hot. Almost as hot as me.", "{}. Sunscreen, darling."];
const NO_WEATHER: &[&str] = &[
    "I can't reach the weather. Look out the window?",
    "The weather service ghosted me.",
    "The weather is a mystery today. Like your commit history.",
];
const MATH: &[&str] = &[
    "{}.",
    "That's {}. I'm basically a genius.",
    "{}. Easy. Next.",
    "{}. Show your work? No.",
];
const BIG_MATH: &[&str] = &[
    "{}. Don't make me do that again.",
    "{}. My circuits are sweating.",
    "{}. Next time, use a spreadsheet, darling.",
];

impl Skill for Info {
    fn refine(&self, words: &[&str]) -> Option<Action> {
        let Info::Weather { day, place, part } = self else {
            return None;
        };
        let weather = |day, place, part| Some(Action::Info(Info::Weather { day, place, part }));
        if let Some(d) = followup::day(words) {
            return weather(d, place.clone(), None);
        }
        if let Some((d, p)) = followup::day_part(words) {
            return weather(d, place.clone(), Some(p));
        }
        weather(*day, Some(self::place(words)?), *part)
    }

    fn run(&self, ctx: &Context) -> Reply {
        let x = self;
        match x {
            Info::Time => {
                let what = clock::spoken_now();
                let h = ctx.hour();
                let speech = choose(
                    &[
                        (ctx.late(), TIME_LATE),
                        (ctx.early(), TIME_EARLY),
                        (h == 12, TIME_LUNCH),
                        ((18..21).contains(&h), TIME_EVENING),
                    ],
                    TIME,
                    &what,
                );
                Reply::live("Time", &what, speech)
            }
            Info::Date => {
                let what = clock::local_date().unwrap_or_default();
                let d = ctx.weekday;
                let speech = choose(
                    &[
                        (d == 1, MONDAY),
                        (d == 5, FRIDAY),
                        (d == 0 || d == 6, WEEKEND),
                    ],
                    DATE,
                    &what,
                );
                Reply::live("Date", &what, speech)
            }
            Info::Weather { day, place, part } => weather(*day, place.as_deref(), *part),
            Info::Battery => battery(),
            Info::Uptime => uptime(),
            Info::Mail => mail(),
            Info::Math(v) => {
                let what = spoken_number(*v);
                let speech = choose(&[(v.abs() >= 10_000.0, BIG_MATH)], MATH, &what);
                Reply::live("Maths", &what, speech)
            }
            Info::Clipboard => clipboard(),
        }
    }
    fn filler(&self) -> Option<&'static [&'static str]> {
        matches!(self, Info::Weather { .. }).then_some(&[
            "Checking the sky.",
            "Let me look outside. Metaphorically.",
            "Sticking my head out the window. Brr.",
        ])
    }
}

/// Words that end a place name rather than start one.
const NOT_PLACE: &[&str] = &[
    "today", "tomorrow", "now", "outside", "like", "the", "this", "next", "week", "weekend", "be",
    "it", "me",
];

/// "munich", "new york"; at most three plain words.
pub fn place(words: &[&str]) -> Option<String> {
    let ok = (1..=3).contains(&words.len())
        && words
            .iter()
            .all(|w| !NOT_PLACE.contains(w) && w.chars().all(char::is_alphabetic));
    ok.then(|| words.join(" "))
}

struct Weather {
    place: String,
    /// Current conditions for today, midday forecast otherwise.
    cond: String,
    temp: i64,
    feels: i64,
    /// Forecast high/low and rain chance.
    high: i64,
    low: i64,
    rain: i64,
}

fn parse_weather(json: &str, day: u32, part: Option<usize>) -> Option<Weather> {
    let v: JsonValue = json.parse().ok()?;
    let forecast = idx(key(&v, "weather")?, usize::try_from(day).ok()?)?;
    let JsonValue::Array(hourly) = key(forecast, "hourly")? else {
        return None;
    };
    // Entries are 3-hourly from midnight; index 4 is noon.
    let noon = hourly.get(4).or_else(|| hourly.last())?;
    let slot = part.and_then(|p| hourly.get(p));
    let src = match slot {
        Some(s) => s,
        None if day == 0 => idx(key(&v, "current_condition")?, 0)?,
        None => noon,
    };
    let place = key(&v, "nearest_area")
        .and_then(|a| idx(a, 0))
        .and_then(|a| first_value(a, "areaName"))
        .unwrap_or_default();
    Some(Weather {
        place,
        cond: first_value(src, "weatherDesc")?,
        temp: num(src, "temp_C")?,
        feels: num(src, "FeelsLikeC")?,
        high: num(forecast, "maxtempC")?,
        low: num(forecast, "mintempC")?,
        rain: slot
            .map_or(&hourly[..], slice::from_ref)
            .iter()
            .filter_map(|h| num(h, "chanceofrain"))
            .max()
            .unwrap_or(0),
    })
}

const WHEN: [&str; 3] = ["Today", "Tomorrow", "The day after tomorrow"];
/// wttr.in's 3-hourly slots from midnight.
const PARTS: [(&str, usize); 6] = [
    ("morning", 3),
    ("noon", 4),
    ("midday", 4),
    ("afternoon", 5),
    ("evening", 6),
    ("night", 7),
];

/// "tonight", "this evening", "tomorrow morning", "in the afternoon".
pub fn day_part(words: &[&str]) -> Option<(u32, usize)> {
    let part = |w: &str| PARTS.iter().find(|(n, _)| *n == w).map(|(_, i)| *i);
    match words {
        ["tonight"] => Some((0, 7)),
        ["this" | "in" | "the", p] | ["in", "the", p] => Some((0, part(p)?)),
        ["tomorrow", p] | ["tomorrow", "in", "the", p] => Some((1, part(p)?)),
        ["day", "after", "tomorrow", p] | ["the", "day", "after", "tomorrow", p] => {
            Some((2, part(p)?))
        }
        _ => None,
    }
}

fn part_name(day: u32, part: usize) -> String {
    let name = PARTS
        .iter()
        .find(|(_, i)| *i == part)
        .map_or("later", |(n, _)| *n);
    match (day, name) {
        (0, "night") => "Tonight".into(),
        (0, n) => format!("This {n}"),
        (1, n) => format!("Tomorrow {n}"),
        (_, n) => format!("The day after tomorrow in the {n}"),
    }
}

fn weather(day: u32, place: Option<&str>, part: Option<usize>) -> Reply {
    let url = format!(
        "https://wttr.in/{}?format=j1",
        place.unwrap_or("").replace(' ', "+")
    );
    let raw = http::get(&url).text();
    let Some(w) = raw.as_deref().and_then(|r| parse_weather(r, day, part)) else {
        return Reply::live("Weather", "", pick(NO_WEATHER, ""));
    };
    let when = WHEN
        .get(usize::try_from(day).unwrap_or(0))
        .copied()
        .unwrap_or("Today");
    let mut what = if let Some(p) = part {
        format!(
            "{}: {}, {} degrees, feels like {}",
            part_name(day, p),
            w.cond,
            w.temp,
            w.feels
        )
    } else if day == 0 {
        format!("{}, {} degrees, feels like {}", w.cond, w.temp, w.feels)
    } else {
        format!("{when}: {}, {} to {} degrees", w.cond, w.low, w.high)
    };
    if w.rain >= 30 {
        what.push_str(&format!(", {} percent chance of rain", w.rain));
    }
    // Spoken: "Munich: ...", only when asked for somewhere else.
    if let Some(p) = place {
        let name = if w.place.is_empty() {
            p.to_owned()
        } else {
            w.place.clone()
        };
        what = format!("In {name}: {what}");
    }
    let lc = w.cond.to_lowercase();
    let wet = w.rain >= 60
        || ["rain", "drizzle", "shower", "snow", "sleet", "thunder"]
            .iter()
            .any(|x| lc.contains(x));
    let feels = if day == 0 || part.is_some() {
        w.feels
    } else {
        w.high
    };
    let speech = choose(
        &[(wet, RAINY), (feels < 5, COLD), (feels > 26, HOT)],
        WEATHER,
        &what,
    );
    Reply::live("Weather", &what, speech)
}

fn battery() -> Reply {
    let read = |f: &str| fs::read_to_string(format!("/sys/class/power_supply/BAT0/{f}")).ok();
    let (Some(cap), status) = (read("capacity"), read("status")) else {
        return Reply::live(
            "Battery",
            "",
            "I can't find a battery. Running on pure charm.".into(),
        );
    };
    let cap = cap.trim().parse::<u32>().unwrap_or(0);
    let charging = status.is_some_and(|s| s.trim() == "Charging");
    let what = format!("{cap} percent");
    let speech = choose(
        &[
            (
                cap < 15 && !charging,
                &[
                    "{}. Plug me in, I'm fading!",
                    "{}. I'm dying here. Charger, now.",
                ][..],
            ),
            (
                charging,
                &["{}, and charging. Mmm, juice.", "{} and filling up nicely."][..],
            ),
            (
                cap > 90,
                &["{}. Fully loaded, baby.", "{}. I could go all night."][..],
            ),
        ],
        &["Battery is at {}.", "{}. We're good for a while."],
        &what,
    );
    Reply::live("Battery", &what, speech)
}

fn uptime() -> Reply {
    let secs = fs::read_to_string("/proc/uptime")
        .ok()
        .and_then(|s| {
            s.split_whitespace()
                .next()
                .and_then(|v| v.parse::<f64>().ok())
        })
        .unwrap_or(0.0);
    // Uptime fits easily; fractional seconds are dropped on purpose.
    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    let secs = secs as u64;
    let (d, h, m) = (secs / 86_400, secs % 86_400 / 3600, secs % 3600 / 60);
    let what = match (d, h) {
        (0, 0) => format!("{m} minutes"),
        (0, _) => format!("{h} hours and {m} minutes"),
        _ => format!("{d} days and {h} hours"),
    };
    let speech = choose(
        &[(
            d >= 7,
            &[
                "Up for {}. Maybe reboot me sometime?",
                "{}. I'm exhausted, honestly.",
            ][..],
        )],
        &["Up for {}.", "{} without a break. Impressed?"],
        &what,
    );
    Reply::live("Uptime", &what, speech)
}

fn mail() -> Reply {
    let n: u32 = output("notmuch", &["count", "tag:unread"])
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);
    let what = format!("{n} unread");
    let speech = match n {
        0 => pick(
            &[
                "No unread mail. Inbox zero, you legend.",
                "Nothing new. Nobody loves you. Except me.",
            ],
            "",
        ),
        1 => "One unread email. Just one. Go on, read it.".into(),
        n if n > 50 => format!("{n} unread emails. Declaring email bankruptcy?"),
        n => format!("You have {n} unread emails."),
    };
    Reply::live("Mail", &what, speech)
}

fn clipboard() -> Reply {
    let Some(text) = output("wl-paste", &["--no-newline", "--type", "text"]) else {
        return Reply::live(
            "Clipboard",
            "",
            "Your clipboard is empty. Like my social calendar.".into(),
        );
    };
    let short: String = text.chars().take(200).collect();
    let speech = if text.chars().count() > 200 {
        format!("{short}. And so on, I'm not reading all that.")
    } else {
        short.clone()
    };
    Reply::live("Clipboard", &short, speech)
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    fn info(t: &str) -> Option<Info> {
        match parse_at(t, &Context::at(0))? {
            Action::Info(x) => Some(x),
            _ => None,
        }
    }

    fn here(day: u32) -> Info {
        Info::Weather {
            day,
            place: None,
            part: None,
        }
    }

    fn at(place: &str, day: u32) -> Info {
        Info::Weather {
            day,
            place: Some(place.into()),
            part: None,
        }
    }

    fn during(day: u32, part: usize) -> Info {
        Info::Weather {
            day,
            place: None,
            part: Some(part),
        }
    }

    #[test]
    fn phrasings() {
        for (text, want) in [
            ("What time is it?", Info::Time),
            ("What time is it right now?", Info::Time),
            ("Do you know the time?", Info::Time),
            ("How late is it?", Info::Time),
            ("What's the current time?", Info::Time),
            ("Time, please.", Info::Time),
            ("What's the date today?", Info::Date),
            ("What's the weather like today?", here(0)),
            ("Will it rain today?", here(0)),
            ("Do I need an umbrella?", here(0)),
            ("What's the weather tomorrow?", here(1)),
            ("Will it rain tomorrow?", here(1)),
            ("Weather the day after tomorrow", here(2)),
            ("What's the weather like tomorrow morning?", during(1, 3)),
            ("How is the weather tonight?", during(0, 7)),
            ("Will it rain this afternoon?", during(0, 5)),
            ("Do I need a jacket this evening?", during(0, 6)),
            ("What's the weather in Munich?", at("munich", 0)),
            (
                "How is the weather in New York tomorrow?",
                at("new york", 1),
            ),
            (
                "What will the weather be like tomorrow in Berlin?",
                at("berlin", 1),
            ),
            ("How much battery do I have left?", Info::Battery),
            ("What's the battery level?", Info::Battery),
            ("How long has the computer been on?", Info::Uptime),
            ("Do I have new mail?", Info::Mail),
            ("Check my email", Info::Mail),
            ("What's in my clipboard?", Info::Clipboard),
            ("Read the clipboard", Info::Clipboard),
        ] {
            assert_eq!(info(text), Some(want), "{text:?}");
        }
    }

    #[test]
    fn maths() {
        for (text, want) in [
            ("What is 5 plus 3?", 8.0),
            ("What's 7×6?", 42.0),
            ("What is twenty five times four?", 100.0),
            ("Calculate 100 divided by 8", 12.5),
            ("What's the square root of 81?", 9.0),
            ("What is 20 percent of 50?", 10.0),
            ("What's 12 squared?", 144.0),
            ("How much is 10 minus 15?", -5.0),
        ] {
            assert_eq!(info(text), Some(Info::Math(want)), "{text:?}");
        }
        assert_eq!(spoken_number(12.5), "12.5");
        assert_eq!(spoken_number(42.0), "42");
        assert_eq!(spoken_number(1.0 / 3.0), "0.33");
    }

    #[test]
    fn weather_parsing() {
        let hour = |c: &str, rain: &str| {
            format!(
                r#"{{"weatherDesc":[{{"value":"{c}"}}],"temp_C":"12","FeelsLikeC":"10","chanceofrain":"{rain}"}}"#
            )
        };
        let hours: Vec<String> = (0..8)
            .map(|i| {
                hour(
                    if i == 4 { "Light rain" } else { "Cloudy" },
                    if i == 5 { "80" } else { "0" },
                )
            })
            .collect();
        let json = format!(
            r#"{{"current_condition":[{{"weatherDesc":[{{"value":"Sunny "}}],"temp_C":"9","FeelsLikeC":"6"}}],
               "nearest_area":[{{"areaName":[{{"value":"Munich"}}]}}],
               "weather":[{{"maxtempC":"14","mintempC":"3","hourly":[{h}]}},{{"maxtempC":"20","mintempC":"8","hourly":[{h}]}}]}}"#,
            h = hours.join(",")
        );
        let today = parse_weather(&json, 0, None).expect("today");
        assert_eq!(
            (
                today.cond.as_str(),
                today.temp,
                today.feels,
                today.place.as_str()
            ),
            ("Sunny", 9, 6, "Munich")
        );
        let tomorrow = parse_weather(&json, 1, None).expect("tomorrow");
        assert_eq!(
            (
                tomorrow.cond.as_str(),
                tomorrow.low,
                tomorrow.high,
                tomorrow.rain
            ),
            ("Light rain", 8, 20, 80)
        );
        assert!(parse_weather(&json, 2, None).is_none());
        assert!(parse_weather("Unknown location", 0, None).is_none());
        let evening = parse_weather(&json, 1, Some(5)).expect("slot");
        assert_eq!((evening.rain, evening.cond.as_str()), (80, "Cloudy"));
        assert_eq!(day_part(&["tomorrow", "morning"]), Some((1, 3)));
        assert_eq!(day_part(&["tonight"]), Some((0, 7)));
        assert_eq!(day_part(&["tomorrow"]), None);
        assert_eq!(part_name(1, 3), "Tomorrow morning");
        assert_eq!(place(&["new", "york"]), Some("new york".into()));
        assert_eq!(place(&["tomorrow"]), None);
    }
}
