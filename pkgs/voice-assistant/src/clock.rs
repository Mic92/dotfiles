//! Spoken clock times: "7:30 pm", "half past four", "noon", "seven oh five".

use crate::duration::number;
use std::mem;
use std::ptr;

pub const DAY: u32 = 24 * 3600;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Clock {
    /// Seconds since midnight.
    pub secs: u32,
    /// No am/pm given for 1..=12, so the time might also be 12 hours later.
    pub twelve_hour: bool,
    /// Unmistakably a time of day ("pm", "o'clock", "past"), not a duration.
    pub marked: bool,
}

impl Clock {
    /// Seconds until the next occurrence; a time equal to now means tomorrow.
    pub fn seconds_from(&self, now: u32) -> u32 {
        let until = |t: u32| match (t + DAY - now % DAY) % DAY {
            0 => DAY,
            d => d,
        };
        let first = until(self.secs);
        if self.twelve_hour {
            first.min(until((self.secs + DAY / 2) % DAY))
        } else {
            first
        }
    }

    /// Seconds until this time `day` days from today. Without am/pm the
    /// earlier reading wins for future days ("tomorrow at 7" is morning) and
    /// the next one still ahead for today; `None` if it already passed.
    pub fn seconds_on(&self, now: u32, day: u32) -> Option<u32> {
        let now = now % DAY;
        if day > 0 {
            return Some(day * DAY + self.secs - now);
        }
        let later = self.twelve_hour.then_some(self.secs + DAY / 2);
        [Some(self.secs), later]
            .into_iter()
            .flatten()
            .find(|t| *t > now)
            .map(|t| t - now)
    }
}

#[derive(Clone, Copy, PartialEq)]
enum Half {
    Am,
    Pm,
}

/// Strip a trailing am/pm or o'clock style qualifier.
fn qualifier<'a>(words: &'a [&'a str]) -> (&'a [&'a str], Option<Half>, bool) {
    let ends = |suffix: &[&str]| {
        words
            .ends_with(suffix)
            .then(|| &words[..words.len() - suffix.len()])
    };
    for (suffix, half) in [
        (&["am"][..], Some(Half::Am)),
        (&["pm"], Some(Half::Pm)),
        (&["in", "the", "morning"], Some(Half::Am)),
        (&["in", "the", "afternoon"], Some(Half::Pm)),
        (&["in", "the", "evening"], Some(Half::Pm)),
        (&["at", "night"], Some(Half::Pm)),
        (&["tonight"], Some(Half::Pm)),
        (&["o'clock"], None),
    ] {
        if let Some(rest) = ends(suffix) {
            return (rest, half, true);
        }
    }
    (words, None, false)
}

/// "7", "7:30", "seven thirty", "seven oh five", "19 45".
fn hour_minute(words: &[&str]) -> Option<(u32, u32)> {
    let minutes = |ws: &[&str]| -> Option<u32> {
        match ws {
            [] => Some(0),
            ["oh" | "o", m] => number(m).filter(|m| *m < 10),
            [tens, ones] => {
                let (t, o) = (number(tens)?, number(ones)?);
                (t % 10 == 0 && t >= 20 && o < 10).then_some(t + o)
            }
            [m] => number(m).filter(|m| *m < 60),
            _ => None,
        }
    };
    let (h, m) = match words {
        [hm] if hm.contains(':') => {
            let (h, m) = hm.split_once(':')?;
            (h.parse().ok()?, m.parse().ok().filter(|m: &u32| *m < 60)?)
        }
        [h, rest @ ..] => (number(h)?, minutes(rest)?),
        [] => return None,
    };
    (h < 24).then_some((h, m))
}

/// "half past four", "quarter to five", "ten past nine", "20 minutes to 6".
fn relative(words: &[&str]) -> Option<(u32, u32)> {
    let idx = words
        .iter()
        .position(|w| matches!(*w, "past" | "after" | "to" | "before"))?;
    let (before, after) = (&words[..idx], &words[idx + 1..]);
    let before = before.strip_prefix(&["a"]).unwrap_or(before);
    let before = before
        .strip_suffix(&["minutes"])
        .or_else(|| before.strip_suffix(&["minute"]))
        .unwrap_or(before);
    let offset = match before {
        ["half"] => 30,
        ["quarter"] => 15,
        [n] => number(n).filter(|n| (1..60).contains(n))?,
        [t, o] => number(t)? + number(o).filter(|o| *o < 10)?,
        _ => return None,
    };
    let [hour] = after else { return None };
    let hour = number(hour).filter(|h| *h < 24)?;
    Some(if matches!(words[idx], "past" | "after") {
        (hour, offset)
    } else if offset == 30 {
        return None; // "half to" is not English
    } else {
        ((hour + 23) % 24, 60 - offset)
    })
}

pub fn parse(words: &[&str]) -> Option<Clock> {
    match words {
        ["noon" | "midday"] => {
            return Some(Clock {
                secs: 12 * 3600,
                twelve_hour: false,
                marked: true,
            });
        }
        ["midnight"] => {
            return Some(Clock {
                secs: 0,
                twelve_hour: false,
                marked: true,
            });
        }
        _ => {}
    }
    let (core, half, qualified) = qualifier(words);
    let (rel, (mut h, m)) = match relative(core) {
        Some(hm) => (true, hm),
        None => (false, hour_minute(core)?),
    };
    match half {
        Some(_) if h == 0 || h > 12 => return None,
        Some(Half::Pm) if h < 12 => h += 12,
        Some(Half::Am) if h == 12 => h = 0,
        _ => {}
    }
    Some(Clock {
        secs: h * 3600 + m * 60,
        twelve_hour: half.is_none() && (1..=12).contains(&h),
        marked: qualified || rel,
    })
}

fn local_tm() -> Option<libc::tm> {
    // SAFETY: time(NULL) has no preconditions; localtime_r writes only into
    // the zeroed `tm` we own and returns null on failure, which we check.
    unsafe {
        let t = libc::time(ptr::null_mut());
        let mut tm: libc::tm = mem::zeroed();
        (!libc::localtime_r(&raw const t, &raw mut tm).is_null()).then_some(tm)
    }
}

fn field(v: i32) -> u32 {
    u32::try_from(v).unwrap_or(0)
}

/// Seconds since local midnight.
pub fn local_now() -> u32 {
    local_tm().map_or(0, |tm| {
        field(tm.tm_hour) * 3600 + field(tm.tm_min) * 60 + field(tm.tm_sec).min(59)
    })
}

/// 0 = Sunday.
pub fn local_weekday() -> u32 {
    local_tm().map_or(0, |tm| field(tm.tm_wday))
}

/// "Thursday, October 23", for replies.
pub fn local_date() -> Option<String> {
    const DAYS: [&str; 7] = [
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
    ];
    const MONTHS: [&str; 12] = [
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December",
    ];
    let tm = local_tm()?;
    let day = DAYS.get(usize::try_from(tm.tm_wday).ok()?)?;
    let month = MONTHS.get(usize::try_from(tm.tm_mon).ok()?)?;
    Some(format!("{day}, {month} {}", tm.tm_mday))
}

pub fn format_hm(secs: u32) -> String {
    let (h, m) = (secs / 3600, secs % 3600 / 60);
    let suffix = if h < 12 { "AM" } else { "PM" };
    let h12 = if h % 12 == 0 { 12 } else { h % 12 };
    if m == 0 {
        format!("{h12} {suffix}")
    } else {
        format!("{h12}:{m:02} {suffix}")
    }
}

/// The current time, "3:45 PM" style.
pub fn spoken_now() -> String {
    format_hm(local_now())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn at(t: &str) -> Option<(u32, bool)> {
        let words: Vec<&str> = t.split_whitespace().collect();
        parse(&words).map(|c| (c.secs / 60, c.marked))
    }

    #[test]
    fn forms() {
        let hm = |h: u32, m: u32| h * 60 + m;
        assert_eq!(at("7:30"), Some((hm(7, 30), false)));
        assert_eq!(at("7:30 pm"), Some((hm(19, 30), true)));
        assert_eq!(at("12 am"), Some((0, true)));
        assert_eq!(at("12 pm"), Some((hm(12, 0), true)));
        assert_eq!(at("six o'clock"), Some((hm(6, 0), true)));
        assert_eq!(at("seven thirty in the evening"), Some((hm(19, 30), true)));
        assert_eq!(at("seven oh five"), Some((hm(7, 5), false)));
        assert_eq!(at("seven forty five"), Some((hm(7, 45), false)));
        assert_eq!(at("half past four"), Some((hm(4, 30), true)));
        assert_eq!(at("quarter to five"), Some((hm(4, 45), true)));
        assert_eq!(at("a quarter past nine"), Some((hm(9, 15), true)));
        assert_eq!(at("ten past nine"), Some((hm(9, 10), true)));
        assert_eq!(at("20 minutes to 6"), Some((hm(5, 40), true)));
        assert_eq!(at("19:45"), Some((hm(19, 45), false)));
        assert_eq!(at("noon"), Some((hm(12, 0), true)));
        assert_eq!(at("midnight"), Some((0, true)));
    }

    #[test]
    fn rejects() {
        for t in [
            "",
            "25:00",
            "7:60",
            "13 pm",
            "half to five",
            "five minutes",
            "seven thirty five forty",
        ] {
            assert_eq!(at(t), None, "{t:?}");
        }
    }

    #[test]
    fn next_occurrence() {
        let now = 14 * 3600; // 14:00
        let c =
            |t: &str| parse(&t.split_whitespace().collect::<Vec<_>>()).map(|c| c.seconds_from(now));
        assert_eq!(c("7:30"), Some(5 * 3600 + 1800)); // 19:30 today
        assert_eq!(c("7:30 am"), Some(17 * 3600 + 1800)); // tomorrow
        assert_eq!(c("3 pm"), Some(3600));
        assert_eq!(c("2 pm"), Some(24 * 3600));
        assert_eq!(c("noon"), Some(22 * 3600));
        assert_eq!(c("1 o'clock"), Some(11 * 3600)); // 01:00 beats 13:00 tomorrow
    }

    #[test]
    fn on_day() {
        let now = 14 * 3600;
        let c = |t: &str, d| {
            parse(&t.split_whitespace().collect::<Vec<_>>()).and_then(|c| c.seconds_on(now, d))
        };
        assert_eq!(c("7", 1), Some(17 * 3600));
        assert_eq!(c("7 pm", 1), Some(29 * 3600));
        assert_eq!(c("9", 0), Some(7 * 3600));
        assert_eq!(c("9 am", 0), None);
        assert_eq!(c("1 pm", 0), None);
    }

    #[test]
    fn format() {
        assert_eq!(format_hm(19 * 3600 + 30 * 60), "7:30 PM");
        assert_eq!(format_hm(12 * 3600), "12 PM");
        assert_eq!(format_hm(5 * 60), "12:05 AM");
    }
}
