//! Maths and unit conversion, worked out by numbat.

use super::{Action, Context, Intent, Module, Reply, Skill, choose, intent, output, pick};
use crate::clock;
use crate::duration::number_words;
use crate::grammar::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Calc {
    /// A numbat expression: "5 + 3", "sqrt(81)".
    Math(String),
    /// A time of day (here when `at` is unset, now) from one zone to
    /// another. Zones index `ZONES`, `None` is here.
    Time {
        at: Option<u32>,
        from: Option<usize>,
        to: Option<usize>,
    },
    /// "What timezone is London in?", an index into `ZONES`.
    Zone(usize),
    /// Indices into `UNITS` of the same dimension.
    Convert { value: f64, from: usize, to: usize },
}

fn c(x: Calc) -> Option<Action> {
    Some(Action::Calc(x))
}

pub const MODULE: Module = Module {
    intents,
    slots: &[
        ("expr", |w, _| expression(w).map(Value::Text)),
        ("from", |w, _| lookup(w).map(Value::Index)),
        ("to", |w, _| lookup(w).map(Value::Index)),
        ("zone", |w, _| zone(w).map(Value::Index)),
        ("origin", |w, _| zone(w).map(Value::Index)),
    ],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            &[
                "![tell me|give me|i need|i need to know|can you tell me|do you know|i would like to know|i'd like to know] [what's|what is] [the] (timezone|time zone) [now|currently] (of|for|in|where) {zone} [is]",
                "!(what|which) (timezone|time zone) (is|does|do) {zone} [use|in|located in|located|reside in|be in]",
                "!(in which|in what) (timezone|time zone) (is|does) {zone} [located|reside|lie]",
                "![what's|what is|tell me|can you tell me|do you know|i need|i need to know] {zone} (timezone|time zone) [is what]",
            ],
            |s, _| match s.get("zone")? {
                Value::Index(z) => c(Calc::Zone(*z)),
                _ => None,
            },
        ),
        intent(
            &[
                "![tell me] (what time is it|what's the time|what is the time|what's the current time|what is the current time|the time) [now|right now] in {zone} [right now|now]",
                "![tell me] (what time is it|what's the time|what is the time|what's the current time) [now|right now] in {zone} [right now|now]",
                "!(what's|what is|what time is) {time} [here|my time|local time] in {zone} [time]",
                "!(what's|what is|what time is) {time} (in|at) {origin} [time] (in|to|for) {zone} [time]",
                "!(what's|what is|what time is) {time} {origin} time (in|to|for) {zone} [time]",
                "!convert {time} [in] {origin} [time] (to|into) {zone} [time]",
                "!convert {time} [here|my time|local time] (to|into) {zone} [time]",
                "!(when|if) it's {time} in {origin} [then] what time is it [here|for me]",
                "!(when|if) it's {time} in {origin} [then] what time is it in {zone}",
                "!(when|if) it's {time} [here] [then] what time is it in {zone}",
                "!what time (will|would|is) it be in {zone} (at|when it's) {time} [here|my time]",
            ],
            |s, _| {
                let at = match s.get("time") {
                    Some(Value::Clock(c)) => Some(c.secs),
                    None => None,
                    Some(_) => return None,
                };
                let z = |k| match s.get(k) {
                    Some(Value::Index(i)) => Some(*i),
                    _ => None,
                };
                let (from, to) = (z("origin"), z("zone"));
                (from.is_some() || to.is_some()).then_some(())?;
                c(Calc::Time { at, from, to })
            },
        ),
        intent(
            &[
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (what's|whats|what is|what are|how much is|how many) {number} {from} (in|to|into|as) {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] what would {number} {from} be in {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] [can you] [assist me|help me] [i need|i want] [to] convert {number} {from} (to|into|in) {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] {number} {from} (in|to|into) {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] {number} {from} (is|are) (equal to|how many) [how many] {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (how many|what amount of) {to} (are|is|will make|make|makes|make up|make up in) [there] [in] [a|an|one] {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (how many|what amount of) {to} (are|is|will make|make|makes|make up|make up in) [there] [in] {number} {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (how many|what amount of) {to} [are|is] [there] in [a|an|one] {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (how many|what amount of) {to} [are|is] [there] in {number} {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (how many|what amount of) {to} (are|is) equivalent to [a|an|one] {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (how many|what amount of) {to} (are|is) equivalent to {number} {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (what's|what is) the [total|correct] (number|amount) of {to} in [a|an|one] {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (what's|what is) the [total|correct] (number|amount) of {to} in {number} {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] [how] (do|can|could|would) (i|we|you) (convert|change) [from] {from} (to|into) {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] [can you] [assist me|help me] [i need|i want] [to] convert [from] {from} (to|into) {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (give me|i need) {from} (to|in) {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] [what is|what's|what would be|give me|i need] the conversion (between|from) [a|an] {from} (and|to) [a|an] {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] if i have {number} {from} how many {to} is that",
            ],
            |s, _| {
                let value = match s.get("number") {
                    Some(Value::Real(v)) => *v,
                    None => 1.0,
                    Some(_) => return None,
                };
                let (Some(Value::Index(from)), Some(Value::Index(to))) =
                    (s.get("from"), s.get("to"))
                else {
                    return None;
                };
                let (from, to) = fluid(*from, *to);
                convertible(from, to).then_some(())?;
                c(Calc::Convert { value, from, to })
            },
        ),
        intent(
            &["!(what's|what|calculate|compute|how) [is|much|many] [is] [the] {expr}"],
            |s, _| match s.get("expr")? {
                Value::Text(e) => c(Calc::Math(e.clone())),
                _ => None,
            },
        ),
    ]
}

/// Spoken places, IANA zone, and how we name it.
pub const ZONES: &[(&[&str], &str, &str)] = &[
    (
        &[
            "new york",
            "nyc",
            "new york city",
            "east coast",
            "eastern time",
            "boston",
            "washington",
            "miami",
            "atlanta",
        ],
        "America/New_York",
        "New York",
    ),
    (
        &[
            "los angeles",
            "la",
            "san francisco",
            "california",
            "seattle",
            "west coast",
            "pacific time",
            "silicon valley",
            "las vegas",
        ],
        "America/Los_Angeles",
        "California",
    ),
    (
        &[
            "chicago",
            "texas",
            "houston",
            "dallas",
            "austin",
            "central time",
        ],
        "America/Chicago",
        "Chicago",
    ),
    (
        &["denver", "mountain time", "colorado"],
        "America/Denver",
        "Denver",
    ),
    (&["phoenix", "arizona"], "America/Phoenix", "Arizona"),
    (&["utah", "salt lake city"], "America/Denver", "Utah"),
    (&["orlando", "florida"], "America/New_York", "Florida"),
    (
        &["missouri", "st louis", "kansas city"],
        "America/Chicago",
        "Missouri",
    ),
    (
        &["british columbia"],
        "America/Vancouver",
        "British Columbia",
    ),
    (
        &[
            "vietnam",
            "hanoi",
            "saigon",
            "ho chi minh",
            "ho chi minh city",
        ],
        "Asia/Ho_Chi_Minh",
        "Vietnam",
    ),
    (&["valencia"], "Europe/Madrid", "Valencia"),
    (&["hawaii", "honolulu"], "Pacific/Honolulu", "Hawaii"),
    (
        &["toronto", "montreal", "canada"],
        "America/Toronto",
        "Toronto",
    ),
    (&["vancouver"], "America/Vancouver", "Vancouver"),
    (
        &["mexico", "mexico city"],
        "America/Mexico_City",
        "Mexico City",
    ),
    (
        &["brazil", "sao paulo", "rio", "rio de janeiro"],
        "America/Sao_Paulo",
        "São Paulo",
    ),
    (
        &["argentina", "buenos aires"],
        "America/Argentina/Buenos_Aires",
        "Buenos Aires",
    ),
    (
        &[
            "london",
            "uk",
            "england",
            "britain",
            "the uk",
            "scotland",
            "edinburgh",
        ],
        "Europe/London",
        "London",
    ),
    (&["dublin", "ireland"], "Europe/Dublin", "Dublin"),
    (&["lisbon", "portugal"], "Europe/Lisbon", "Lisbon"),
    (&["paris", "france"], "Europe/Paris", "Paris"),
    (
        &["berlin", "germany", "munich", "hamburg", "frankfurt"],
        "Europe/Berlin",
        "Germany",
    ),
    (
        &["amsterdam", "netherlands", "holland"],
        "Europe/Amsterdam",
        "Amsterdam",
    ),
    (&["madrid", "spain", "barcelona"], "Europe/Madrid", "Spain"),
    (&["rome", "italy", "milan"], "Europe/Rome", "Italy"),
    (&["vienna", "austria"], "Europe/Vienna", "Vienna"),
    (&["zurich", "switzerland"], "Europe/Zurich", "Zurich"),
    (&["stockholm", "sweden"], "Europe/Stockholm", "Stockholm"),
    (&["helsinki", "finland"], "Europe/Helsinki", "Helsinki"),
    (&["warsaw", "poland"], "Europe/Warsaw", "Warsaw"),
    (&["athens", "greece"], "Europe/Athens", "Athens"),
    (&["kyiv", "kiev", "ukraine"], "Europe/Kyiv", "Kyiv"),
    (&["istanbul", "turkey"], "Europe/Istanbul", "Istanbul"),
    (&["moscow", "russia"], "Europe/Moscow", "Moscow"),
    (
        &["israel", "tel aviv", "jerusalem"],
        "Asia/Jerusalem",
        "Israel",
    ),
    (&["cairo", "egypt"], "Africa/Cairo", "Cairo"),
    (
        &["south africa", "johannesburg", "cape town"],
        "Africa/Johannesburg",
        "South Africa",
    ),
    (&["nairobi", "kenya"], "Africa/Nairobi", "Nairobi"),
    (&["lagos", "nigeria"], "Africa/Lagos", "Lagos"),
    (&["dubai", "uae", "abu dhabi"], "Asia/Dubai", "Dubai"),
    (
        &["india", "delhi", "new delhi", "mumbai", "bangalore"],
        "Asia/Kolkata",
        "India",
    ),
    (&["bangkok", "thailand"], "Asia/Bangkok", "Bangkok"),
    (&["singapore"], "Asia/Singapore", "Singapore"),
    (&["hong kong"], "Asia/Hong_Kong", "Hong Kong"),
    (&["china", "beijing", "shanghai"], "Asia/Shanghai", "China"),
    (&["taiwan", "taipei"], "Asia/Taipei", "Taipei"),
    (&["tokyo", "japan", "osaka"], "Asia/Tokyo", "Tokyo"),
    (&["seoul", "korea", "south korea"], "Asia/Seoul", "Seoul"),
    (
        &["sydney", "australia", "melbourne"],
        "Australia/Sydney",
        "Sydney",
    ),
    (&["perth"], "Australia/Perth", "Perth"),
    (
        &["new zealand", "auckland"],
        "Pacific/Auckland",
        "New Zealand",
    ),
    (&["utc", "gmt"], "UTC", "UTC"),
    // Abbreviations are fixed offsets: "3 pm EST" is UTC-5 even in summer.
    // Etc/GMT signs are inverted.
    (
        &["cet", "c e t", "central european time"],
        "Etc/GMT-1",
        "CET",
    ),
    (
        &["cest", "c e s t", "central european summer time"],
        "Etc/GMT-2",
        "CEST",
    ),
    (&["wet", "w e t", "western european time"], "Etc/GMT", "WET"),
    (
        &["west", "w e s t", "western european summer time"],
        "Etc/GMT-1",
        "WEST",
    ),
    (
        &["eet", "e e t", "eastern european time"],
        "Etc/GMT-2",
        "EET",
    ),
    (
        &["eest", "e e s t", "eastern european summer time"],
        "Etc/GMT-3",
        "EEST",
    ),
    (&["bst", "b s t", "british summer time"], "Etc/GMT-1", "BST"),
    (&["msk", "m s k", "moscow time"], "Etc/GMT-3", "MSK"),
    (
        &["est", "e s t", "eastern standard time"],
        "Etc/GMT+5",
        "EST",
    ),
    (
        &["edt", "e d t", "eastern daylight time"],
        "Etc/GMT+4",
        "EDT",
    ),
    (
        &["cst", "c s t", "central standard time"],
        "Etc/GMT+6",
        "CST",
    ),
    (
        &["cdt", "c d t", "central daylight time"],
        "Etc/GMT+5",
        "CDT",
    ),
    (
        &["mst", "m s t", "mountain standard time"],
        "Etc/GMT+7",
        "MST",
    ),
    (
        &["mdt", "m d t", "mountain daylight time"],
        "Etc/GMT+6",
        "MDT",
    ),
    (
        &["pst", "p s t", "pacific standard time"],
        "Etc/GMT+8",
        "PST",
    ),
    (
        &["pdt", "p d t", "pacific daylight time"],
        "Etc/GMT+7",
        "PDT",
    ),
    (&["akst", "a k s t", "alaska time"], "Etc/GMT+9", "AKST"),
    (
        &["hst", "h s t", "hawaii standard time"],
        "Etc/GMT+10",
        "HST",
    ),
    (&["jst", "j s t", "japan standard time"], "Etc/GMT-9", "JST"),
    (&["kst", "k s t", "korea standard time"], "Etc/GMT-9", "KST"),
    (&["sgt", "s g t", "singapore time"], "Etc/GMT-8", "SGT"),
    (&["hkt", "h k t", "hong kong time"], "Etc/GMT-8", "HKT"),
    (
        &["aest", "a e s t", "australian eastern standard time"],
        "Etc/GMT-10",
        "AEST",
    ),
    (
        &["aedt", "a e d t", "australian eastern daylight time"],
        "Etc/GMT-11",
        "AEDT",
    ),
    (
        &["nzst", "n z s t", "new zealand standard time"],
        "Etc/GMT-12",
        "NZST",
    ),
    (
        &["nzdt", "n z d t", "new zealand daylight time"],
        "Etc/GMT-13",
        "NZDT",
    ),
    (
        &["ist", "i s t", "indian standard time"],
        "Asia/Kolkata",
        "IST",
    ),
];

pub fn zone(words: &[&str]) -> Option<usize> {
    let name = words.join(" ");
    let name = name.strip_suffix("'s").unwrap_or(&name);
    ZONES.iter().position(|(names, _, _)| names.contains(&name))
}

fn tz(z: Option<usize>) -> String {
    match z.and_then(|z| ZONES.get(z)) {
        Some((_, iana, _)) => format!("tz(\"{iana}\")"),
        None => "local".into(),
    }
}

/// `"IST +05:30 01:40"` -> abbreviation, spoken offset, time there.
/// Zones without a name print the offset instead, like `-03`.
fn zone_info(out: &str) -> Option<(String, String, u32)> {
    let mut it = out.trim().trim_matches('"').split_whitespace();
    let abbr = it.next()?;
    let offset = it.next()?;
    let (h, m) = it.next()?.split_once(':')?;
    let secs = h.parse::<u32>().ok()? * 3600 + m.parse::<u32>().ok()? * 60;
    let abbr = if abbr.starts_with(['+', '-']) {
        ""
    } else {
        abbr
    };
    let (sign, rest) = offset.split_at(1);
    let (oh, om) = rest.split_once(':')?;
    let hours = oh.trim_start_matches('0');
    let spoken = match (hours, om) {
        ("", "00") => "UTC".to_owned(),
        (h, "00") => format!("UTC {} {h}", if sign == "-" { "minus" } else { "plus" }),
        (h, m) => format!(
            "UTC {} {}:{m}",
            if sign == "-" { "minus" } else { "plus" },
            if h.is_empty() { "0" } else { h }
        ),
    };
    Some((abbr.to_owned(), spoken, secs))
}

const ZONE_IS: &[&str] = &[
    "{}.",
    "{}. Now you know when to call.",
    "{}. Write it on your hand.",
    "{}. Time zones: humanity's worst invention after Mondays.",
    "{}. Adjust your Club-Mate schedule accordingly.",
    "{}. Long-distance thing going on? Tell me everything.",
];

fn which_zone(z: usize) -> Reply {
    let Some((_, iana, name)) = ZONES.get(z) else {
        return Reply::live("Time zone", "", pick(&["No idea where that is."], ""));
    };
    let e = format!("format_datetime(\"%Z %:z %H:%M\", now() -> tz(\"{iana}\"))");
    let Some((abbr, offset, secs)) = output("numbat", &["-e", &e]).as_deref().and_then(zone_info)
    else {
        return Reply::live(
            "Time zone",
            "",
            pick(&["Time zones broke. Blame the British."], ""),
        );
    };
    let zone = if abbr.is_empty() || abbr == "UTC" {
        offset
    } else {
        format!("{abbr}, {offset}")
    };
    let what = format!("{name} is on {zone}. It's {} there", clock::format_hm(secs));
    Reply::live("Time zone", &what, pick(ZONE_IS, &what))
}

/// numbat prints the start's day of year, then the result's day of year
/// and time, so we can tell "tomorrow" from "today".
fn time_expr(at: Option<u32>, from: Option<usize>, to: Option<usize>) -> String {
    let start = match at {
        None => "now()".to_owned(),
        Some(secs) => {
            let zone = from
                .and_then(|z| ZONES.get(z))
                .map_or(String::new(), |(_, iana, _)| format!(" {iana}"));
            format!(
                "datetime(\"{{format_datetime(\"%Y-%m-%d\", now() -> {})}} {:02}:{:02}{zone}\")",
                tz(from),
                secs / 3600,
                secs / 60 % 60
            )
        }
    };
    format!(
        "\"{{format_datetime(\"%j\", {start})}} {{format_datetime(\"%j %H:%M\", {start} -> {})}}\"",
        tz(to)
    )
}

/// `"298 299 21:05"` -> 21:05, one day later.
fn shifted(out: &str) -> Option<(u32, i32)> {
    let out = out.trim().trim_matches('"');
    let mut it = out.split_whitespace();
    let a: i32 = it.next()?.parse().ok()?;
    let b: i32 = it.next()?.parse().ok()?;
    let (h, m) = it.next()?.split_once(':')?;
    let secs = h.parse::<u32>().ok()? * 3600 + m.parse::<u32>().ok()? * 60;
    let shift = match b - a {
        d if d > 1 => -1,
        d if d < -1 => 1,
        d => d,
    };
    Some((secs, shift))
}

fn place_name(z: Option<usize>) -> &'static str {
    z.and_then(|z| ZONES.get(z))
        .map_or("here", |(_, _, name)| name)
}

const ZONE_NOW: &[&str] = &[
    "{}.",
    "{}. Hope they're awake.",
    "{}. Don't call them if it's the middle of the night, monster.",
    "{}. Time is relative anyway.",
    "{}. Somewhere it's always Club-Mate o'clock.",
    "{}. Jet lag not included.",
    "{}. Planning a call or an escape?",
    "{}. Different place, same deadlines.",
    "{}. Perfect time for a booty call? I'm not judging.",
    "{}. I checked, the planet's still spinning.",
];
const ZONE_NOW_LATE: &[&str] = &[
    "{}. Who are you texting at this hour?",
    "{}. At least someone over there is awake.",
    "{}. Go to sleep, citizen of the world.",
];

fn world_time(at: Option<u32>, from: Option<usize>, to: Option<usize>, ctx: &Context) -> Reply {
    let out = output("numbat", &["-e", &time_expr(at, from, to)]);
    let Some((secs, shift)) = out.as_deref().and_then(shifted) else {
        return Reply::live(
            "Time",
            "",
            pick(&["Time zones broke. Blame the British."], ""),
        );
    };
    let day = match shift {
        1 => " tomorrow",
        -1 => " yesterday",
        _ => "",
    };
    let there = format!(
        "{}{day} {}",
        clock::format_hm(secs),
        match to {
            Some(_) => format!("in {}", place_name(to)),
            None => "here".into(),
        }
    );
    let what = match at {
        None => format!("It's {there}"),
        Some(at) => format!(
            "{} {} is {there}",
            clock::format_hm(at),
            match from {
                Some(_) => format!("in {}", place_name(from)),
                None => "here".into(),
            }
        ),
    };
    Reply::live(
        "Time",
        &what,
        choose(&[(ctx.late(), ZONE_NOW_LATE)], ZONE_NOW, &what),
    )
}

/// One spoken operator and how numbat writes it.
const OPERATORS: &[(&[&str], &str)] = &[
    (&["plus"], " + "),
    (&["+"], " + "),
    (&["minus"], " - "),
    (&["-"], " - "),
    (&["times"], " * "),
    (&["x"], " * "),
    (&["×"], " * "),
    (&["*"], " * "),
    (&["multiplied", "by"], " * "),
    (&["divided", "by"], " / "),
    (&["over"], " / "),
    (&["/"], " / "),
    (&["to", "the", "power", "of"], "^"),
    (&["percent", "of"], " / 100 * "),
];

/// The longest run of number words at the start: "twenty five times".
fn leading_number(words: &[&str]) -> Option<(f64, usize)> {
    (1..=words.len())
        .rev()
        .find_map(|n| number_words(&words[..n]).map(|v| (v, n)))
}

/// "5 plus 3", "square root of 81", "2 to the power of 10" as numbat
/// expressions. Only the shape is checked here, so the grammar can try
/// every span cheaply, and numbat computes it once when answering.
pub fn expression(words: &[&str]) -> Option<String> {
    let mut out = String::new();
    let mut rest = words;
    let mut ops = 0;
    loop {
        let root = rest.starts_with(&["square", "root", "of"]);
        if root {
            rest = &rest[3..];
        }
        let (v, n) = leading_number(rest)?;
        rest = &rest[n..];
        let v = format!("{v}");
        if root {
            out.push_str(&format!("sqrt({v})"));
            ops += 1;
        } else {
            out.push_str(&v);
        }
        loop {
            match rest.first() {
                Some(&"squared") => out.push_str("^2"),
                Some(&"cubed") => out.push_str("^3"),
                _ => break,
            }
            rest = &rest[1..];
            ops += 1;
        }
        if rest == ["percent"] {
            return Some(out + " / 100");
        }
        if rest.is_empty() {
            return (ops > 0).then_some(out);
        }
        let (words, op) = OPERATORS.iter().find(|(w, _)| rest.starts_with(w))?;
        out.push_str(op);
        rest = &rest[words.len()..];
        ops += 1;
    }
}

/// numbat's leading number: "8.04672 km", "37.7778".
fn number(out: &str) -> Option<f64> {
    out.split_whitespace().next()?.replace(',', "").parse().ok()
}

fn numbat(expr: &str) -> Option<f64> {
    number(&output("numbat", &["-e", expr])?)
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

const NO_MATH: &[&str] = &[
    "That doesn't compute. Literally.",
    "I tried. Maths said no.",
    "Undefined. Like your plans for tonight.",
];

impl Skill for Calc {
    fn run(&self, ctx: &Context) -> Reply {
        match self {
            Calc::Math(expr) => match numbat(expr) {
                Some(v) => {
                    let what = spoken_number(v);
                    let speech = choose(&[(v.abs() >= 10_000.0, BIG_MATH)], MATH, &what);
                    Reply::live("Maths", &what, speech)
                }
                None => Reply::live("Maths", "", pick(NO_MATH, "")),
            },
            Calc::Convert { value, from, to } => conversion(*value, *from, *to, ctx),
            Calc::Time { at, from, to } => world_time(*at, *from, *to, ctx),
            Calc::Zone(z) => which_zone(*z),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Dim {
    Length,
    Mass,
    Volume,
    Temperature,
    Speed,
}

pub struct Unit {
    pub singular: &'static str,
    pub plural: &'static str,
    names: &'static [&'static str],
    dim: Dim,
    /// The unit as numbat writes it.
    numbat: &'static str,
}

const fn u(
    singular: &'static str,
    plural: &'static str,
    names: &'static [&'static str],
    dim: Dim,
    numbat: &'static str,
) -> Unit {
    Unit {
        singular,
        plural,
        names,
        dim,
        numbat,
    }
}

use Dim::{Length, Mass, Speed, Temperature, Volume};

pub const UNITS: &[Unit] = &[
    u(
        "millimeter",
        "millimeters",
        &["mm", "millimetre", "millimetres"],
        Length,
        "mm",
    ),
    u(
        "centimeter",
        "centimeters",
        &["cm", "centimetre", "centimetres"],
        Length,
        "cm",
    ),
    u("meter", "meters", &["m", "metre", "metres"], Length, "m"),
    u(
        "kilometer",
        "kilometers",
        &["km", "kilometre", "kilometres", "k"],
        Length,
        "km",
    ),
    u("inch", "inches", &["in"], Length, "inch"),
    u("foot", "feet", &["ft"], Length, "ft"),
    u("yard", "yards", &["yd"], Length, "yard"),
    u("mile", "miles", &["mi"], Length, "mile"),
    u(
        "milligram",
        "milligrams",
        &["mg", "milligramme", "milligrammes"],
        Mass,
        "mg",
    ),
    u("gram", "grams", &["g", "gramme", "grammes"], Mass, "g"),
    u(
        "kilogram",
        "kilograms",
        &["kg", "kilo", "kilos", "kilogramme"],
        Mass,
        "kg",
    ),
    u("ounce", "ounces", &["oz"], Mass, "oz"),
    u("pound", "pounds", &["lb", "lbs"], Mass, "lb"),
    u("stone", "stone", &["stones"], Mass, "stone"),
    u(
        "ton",
        "tons",
        &["tonne", "tonnes", "metric ton", "metric tons"],
        Mass,
        "tonne",
    ),
    u(
        "milliliter",
        "milliliters",
        &["ml", "millilitre", "millilitres"],
        Volume,
        "mL",
    ),
    u("liter", "liters", &["l", "litre", "litres"], Volume, "L"),
    u("teaspoon", "teaspoons", &["tsp"], Volume, "tsp"),
    u("tablespoon", "tablespoons", &["tbsp"], Volume, "tbsp"),
    u("cup", "cups", &[], Volume, "cup"),
    u(
        "fluid ounce",
        "fluid ounces",
        &["fl oz"],
        Volume,
        "fluidounce",
    ),
    u("pint", "pints", &[], Volume, "pint"),
    u("quart", "quarts", &[], Volume, "quart"),
    u("gallon", "gallons", &[], Volume, "gallon"),
    u(
        "degree Celsius",
        "degrees Celsius",
        &[
            "celsius",
            "centigrade",
            "degrees celsius",
            "degree celsius",
            "degrees centigrade",
            "c",
        ],
        Temperature,
        "°C",
    ),
    u(
        "degree Fahrenheit",
        "degrees Fahrenheit",
        &["fahrenheit", "degrees fahrenheit", "degree fahrenheit", "f"],
        Temperature,
        "°F",
    ),
    u("kelvin", "kelvin", &["kelvins"], Temperature, "K"),
    u(
        "kilometer per hour",
        "kilometers per hour",
        &["km/h", "kmh", "kilometres per hour", "kph"],
        Speed,
        "km/h",
    ),
    u("mile per hour", "miles per hour", &["mph"], Speed, "mph"),
    u(
        "meter per second",
        "meters per second",
        &["metres per second"],
        Speed,
        "m/s",
    ),
    u("knot", "knots", &[], Speed, "knot"),
];

/// "kilometers per hour", "a liter", "degrees celsius".
pub fn lookup(words: &[&str]) -> Option<usize> {
    let words = match words {
        ["a" | "an" | "one" | "the", rest @ ..] => rest,
        w => w,
    };
    let name = words.join(" ");
    UNITS.iter().position(|u| {
        u.singular.eq_ignore_ascii_case(&name)
            || u.plural.eq_ignore_ascii_case(&name)
            || u.names.contains(&name.as_str())
    })
}

pub fn convertible(from: usize, to: usize) -> bool {
    matches!((UNITS.get(from), UNITS.get(to)), (Some(f), Some(t)) if f.dim == t.dim)
}

/// numbat's expression for the conversion.
fn conversion_expr(value: f64, from: usize, to: usize) -> Option<String> {
    let (f, t) = (UNITS.get(from)?, UNITS.get(to)?);
    Some(format!("{value} {} -> {}", f.numbat, t.numbat))
}

pub fn convert(value: f64, from: usize, to: usize) -> Option<f64> {
    if !convertible(from, to) {
        return None;
    }
    numbat(&conversion_expr(value, from, to)?)
}

/// "Ounces in a cup" means fluid ounces.
pub fn fluid(from: usize, to: usize) -> (usize, usize) {
    let fix = |a: usize, b: usize| match (UNITS.get(a), UNITS.get(b)) {
        (Some(x), Some(y)) if x.singular == "ounce" && y.dim == Volume => UNITS
            .iter()
            .position(|u| u.singular == "fluid ounce")
            .unwrap_or(a),
        _ => a,
    };
    (fix(from, to), fix(to, from))
}

/// Three significant digits, whole numbers from 100 up.
pub fn spoken(x: f64) -> String {
    let digits = if x.abs() >= 100.0 || x == 0.0 {
        0
    } else {
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        let d = (2.0 - x.abs().log10().floor()) as usize;
        d.min(6)
    };
    let s = format!("{x:.digits$}");
    let s = if s.contains('.') {
        s.trim_end_matches('0').trim_end_matches('.')
    } else {
        &s
    };
    if s == "-0" { "0".into() } else { s.into() }
}

pub fn name(unit: usize, value: f64) -> &'static str {
    UNITS.get(unit).map_or("", |u| {
        if (value - 1.0).abs() < f64::EPSILON {
            u.singular
        } else {
            u.plural
        }
    })
}

const CONVERTED: &[&str] = &[
    "{}.",
    "{}. Science.",
    "{}. I did the maths so you don't have to.",
    "{}. Metric would've been easier, just saying.",
    "{}. You're welcome, Imperial victim.",
    "That's {}. Write it down this time.",
    "{}. No calculator was harmed.",
    "{}. Precise enough for cooking, not for rocket science.",
    "{}. Size isn't everything, but this one's exact.",
    "Roughly {}. Don't sue me over the decimals.",
];
const CONVERTED_LATE: &[&str] = &[
    "{}. Baking at this hour? Respect.",
    "{}. Measuring things at midnight. I won't ask.",
    "{}. Now go to bed.",
];

fn conversion(value: f64, from: usize, to: usize, ctx: &Context) -> Reply {
    let Some(result) = convert(value, from, to) else {
        return Reply::live(
            "Convert",
            "",
            pick(&["Those don't convert. Apples and oranges."], ""),
        );
    };
    let what = format!(
        "{} {} is {} {}",
        spoken(value),
        name(from, value),
        spoken(result),
        name(to, result)
    );
    Reply::live(
        "Convert",
        &what,
        choose(&[(ctx.late(), CONVERTED_LATE)], CONVERTED, &what),
    )
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    fn calc(t: &str) -> Option<Calc> {
        match parse_at(t, &Context::at(0))? {
            Action::Calc(x) => Some(x),
            _ => None,
        }
    }

    fn conv(t: &str) -> Option<(f64, &'static str, &'static str)> {
        match calc(t)? {
            Calc::Convert { value, from, to } => {
                Some((value, UNITS[from].plural, UNITS[to].plural))
            }
            _ => None,
        }
    }

    #[test]
    fn conversions() {
        for (text, want) in [
            ("How many cups are in a liter?", (1.0, "liters", "cups")),
            (
                "convert 5 miles to kilometers",
                (5.0, "miles", "kilometers"),
            ),
            (
                "What is 100 degrees fahrenheit in celsius",
                (100.0, "degrees Fahrenheit", "degrees Celsius"),
            ),
            ("how many ounces in a pound", (1.0, "pounds", "ounces")),
            ("what's 3 feet in centimeters", (3.0, "feet", "centimeters")),
            (
                "how many teaspoons are there in a tablespoon",
                (1.0, "tablespoons", "teaspoons"),
            ),
            (
                "how many ounces are in a cup",
                (1.0, "cups", "fluid ounces"),
            ),
            (
                "how do I convert pounds to kilograms",
                (1.0, "pounds", "kilograms"),
            ),
            (
                "12 feet is equal to how many inches",
                (12.0, "feet", "inches"),
            ),
            (
                "what's the conversion between tablespoons and cups",
                (1.0, "tablespoons", "cups"),
            ),
            (
                "twenty kilometers per hour in miles per hour",
                (20.0, "kilometers per hour", "miles per hour"),
            ),
        ] {
            let (v, from, to) = want;
            assert_eq!(conv(text), Some((v, from, to)), "{text}");
        }
        assert_eq!(conv("how many miles in a kilogram"), None);
    }

    #[test]
    fn maths() {
        for (text, want) in [
            ("What is 5 plus 3?", "5 + 3"),
            ("What's 7×6?", "7 * 6"),
            ("What is twenty five times four?", "25 * 4"),
            ("Calculate 100 divided by 8", "100 / 8"),
            ("What's the square root of 81?", "sqrt(81)"),
            ("What is 20 percent of 50?", "20 / 100 * 50"),
            ("What's 12 squared?", "12^2"),
            ("How much is 10 minus 15?", "10 - 15"),
            ("What is 2 to the power of 10", "2^10"),
            ("what's 3 plus 4 times 2", "3 + 4 * 2"),
        ] {
            assert_eq!(calc(text), Some(Calc::Math(want.into())), "{text:?}");
        }
        assert_eq!(calc("what is the meaning of life"), None);
        assert_eq!(spoken_number(12.5), "12.5");
        assert_eq!(spoken_number(42.0), "42");
        assert_eq!(spoken_number(1.0 / 3.0), "0.33");
    }

    fn convert_live(v: f64, from: &str, to: &str) -> String {
        let (f, t) = (lookup(&[from]).unwrap(), lookup(&[to]).unwrap());
        spoken(convert(v, f, t).unwrap())
    }

    fn tz(t: &str) -> Option<(Option<u32>, Option<&'static str>, Option<&'static str>)> {
        match calc(t)? {
            Calc::Time { at, from, to } => {
                Some((at, from.map(|z| ZONES[z].1), to.map(|z| ZONES[z].1)))
            }
            _ => None,
        }
    }

    #[test]
    fn time_zones() {
        let pm3 = Some(15 * 3600);
        for (text, want) in [
            (
                "What time is it in Tokyo?",
                (None, None, Some("Asia/Tokyo")),
            ),
            (
                "what's the time in new york right now",
                (None, None, Some("America/New_York")),
            ),
            (
                "what time is it in california",
                (None, None, Some("America/Los_Angeles")),
            ),
            (
                "what is 3 pm here in new york",
                (pm3, None, Some("America/New_York")),
            ),
            (
                "what's 3 pm in new york in tokyo time",
                (pm3, Some("America/New_York"), Some("Asia/Tokyo")),
            ),
            (
                "convert 3 pm london time to sydney time",
                (pm3, Some("Europe/London"), Some("Australia/Sydney")),
            ),
            (
                "when it's 3 pm in new york what time is it here",
                (pm3, Some("America/New_York"), None),
            ),
            (
                "what time will it be in india at 3 pm",
                (pm3, None, Some("Asia/Kolkata")),
            ),
        ] {
            assert_eq!(tz(text), Some(want), "{text}");
        }
    }

    #[test]
    fn which_zone_phrasings() {
        let z = |t| match calc(t) {
            Some(Calc::Zone(z)) => Some(ZONES[z].1),
            _ => None,
        };
        for (text, want) in [
            ("what timezone is london in", "Europe/London"),
            ("what's london's timezone", "Europe/London"),
            ("what is the timezone for la", "America/Los_Angeles"),
            ("in which time zone does denver reside", "America/Denver"),
            ("tell me the timezone for hong kong", "Asia/Hong_Kong"),
            ("what timezone is hanoi in", "Asia/Ho_Chi_Minh"),
            ("timezone in salt lake city", "America/Denver"),
        ] {
            assert_eq!(z(text), Some(want), "{text}");
        }
        assert_eq!(
            zone_info("\"GMT +00:00 20:10\""),
            Some(("GMT".into(), "UTC".into(), 20 * 3600 + 600))
        );
        assert_eq!(
            zone_info("\"IST +05:30 01:40\""),
            Some(("IST".into(), "UTC plus 5:30".into(), 6000))
        );
        assert_eq!(
            zone_info("\"-03 -03:00 17:10\""),
            Some((String::new(), "UTC minus 3".into(), 17 * 3600 + 600))
        );
    }

    #[test]
    fn time_zone_expressions() {
        let tokyo = zone(&["tokyo"]);
        let ny = zone(&["new", "york"]);
        assert!(time_expr(None, None, tokyo).contains(r#"now() -> tz("Asia/Tokyo")"#));
        let e = time_expr(Some(15 * 3600 + 30 * 60), ny, None);
        assert!(e.contains(r#"15:30 America/New_York")"#), "{e}");
        assert!(e.contains("-> local"), "{e}");
        assert_eq!(shifted("\"298 299 21:05\""), Some((21 * 3600 + 5 * 60, 1)));
        assert_eq!(shifted("\"1 365 23:00\""), Some((23 * 3600, -1)));
    }

    #[test]
    fn numbat_expressions() {
        let e =
            |v, f: &str, t: &str| conversion_expr(v, lookup(&[f]).unwrap(), lookup(&[t]).unwrap());
        assert_eq!(e(5.0, "miles", "km").as_deref(), Some("5 mile -> km"));
        assert_eq!(
            e(100.0, "fahrenheit", "celsius").as_deref(),
            Some("100 °F -> °C")
        );
        assert_eq!(number("8.04672 km"), Some(8.04672));
        assert_eq!(number("37.7778"), Some(37.7778));
        assert_eq!(number("1,609.34 m"), Some(1609.34));
        assert!(!convertible(
            lookup(&["mile"]).unwrap(),
            lookup(&["gram"]).unwrap()
        ));
    }

    #[test]
    #[ignore = "needs numbat"]
    fn converts() {
        assert_eq!(convert_live(5.0, "miles", "km"), "8.05");
        assert_eq!(convert_live(100.0, "fahrenheit", "celsius"), "37.8");
        assert_eq!(convert_live(0.0, "celsius", "fahrenheit"), "32");
        assert_eq!(numbat("3 + 4 * 2"), Some(11.0));
        assert_eq!(numbat("20 / 100 * 50"), Some(10.0));
        assert_eq!(numbat("2^10"), Some(1024.0));
        assert_eq!(numbat("1 / 0"), None);
        let run = |at, from: &[&str], to: &[&str]| {
            let e = time_expr(at, zone(from), zone(to));
            let out = output("numbat", &["-e", &e]);
            eprintln!("{e}\n=> {out:?}");
            out.as_deref().and_then(shifted)
        };
        assert!(run(Some(15 * 3600), &["new", "york"], &["berlin"]).is_some());
        assert_eq!(
            run(Some(23 * 3600), &["berlin"], &["tokyo"]).map(|r| r.1),
            Some(1)
        );
        assert_eq!(
            run(Some(3600), &["tokyo"], &["london"]).map(|r| r.1),
            Some(-1)
        );
        assert!(run(None, &["here"], &["tokyo"]).is_some());
        let r = which_zone(zone(&["india"]).unwrap());
        eprintln!("{}", r.body);
        assert!(r.body.contains("UTC plus 5:30"));
        assert_eq!(convert_live(1.0, "liter", "cups"), "4.23");
        assert_eq!(convert_live(1.0, "pound", "grams"), "454");
        assert_eq!(convert_live(1.0, "inch", "mm"), "25.4");
    }
}
