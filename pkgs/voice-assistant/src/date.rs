//! Spoken calendar dates: "friday", "next friday", "march 1st", "the 15th".
//! Parsed without knowing today, resolved against it later, so parsing
//! stays the same whatever the date.

use crate::duration::number;
use crate::holiday::{civil, day_number, weekday};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Date {
    /// 0 = Sunday. `next` skips this week's: "next friday".
    Weekday { day: u32, next: bool },
    /// 1-based month.
    Month { month: u32, day: u32 },
    /// "the 15th": this month, or the next one once it passed.
    OfMonth(u32),
}

const WEEKDAYS: [&str; 7] = [
    "sunday",
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
];

const MONTHS: [&str; 12] = [
    "january",
    "february",
    "march",
    "april",
    "may",
    "june",
    "july",
    "august",
    "september",
    "october",
    "november",
    "december",
];

const ORDINALS: [&str; 19] = [
    "first",
    "second",
    "third",
    "fourth",
    "fifth",
    "sixth",
    "seventh",
    "eighth",
    "ninth",
    "tenth",
    "eleventh",
    "twelfth",
    "thirteenth",
    "fourteenth",
    "fifteenth",
    "sixteenth",
    "seventeenth",
    "eighteenth",
    "nineteenth",
];

/// Position of `w` in `names`, plus `first`.
fn index(names: &[&str], w: &str, first: u32) -> Option<u32> {
    let i = names.iter().position(|n| *n == w)?;
    Some(u32::try_from(i).ok()? + first)
}

/// "1st", "15", "first", "twenty first", "thirtieth".
fn day_of_month(words: &[&str]) -> Option<u32> {
    let ordinal = |w: &str| index(&ORDINALS, w, 1);
    let n = match words {
        [w] => {
            let digits = w.trim_end_matches(|c: char| c.is_ascii_alphabetic());
            if digits.is_empty() {
                ordinal(w)
                    .or(match *w {
                        "twentieth" => Some(20),
                        "thirtieth" => Some(30),
                        _ => None,
                    })
                    .or_else(|| number(w))?
            } else {
                let suffix = &w[digits.len()..];
                if !matches!(suffix, "" | "st" | "nd" | "rd" | "th") {
                    return None;
                }
                digits.parse().ok()?
            }
        }
        // `normalize` splits "1st" into "1 st".
        [d, "st" | "nd" | "rd" | "th"] => d.parse().ok()?,
        ["twenty", w] => 20 + ordinal(w).filter(|n| *n < 10)?,
        ["thirty", "first"] => 31,
        _ => return None,
    };
    (1..=31).contains(&n).then_some(n)
}

/// "march", or abbreviated: "sept", "dec".
fn month(w: &str) -> Option<u32> {
    let i = MONTHS
        .iter()
        .position(|m| *m == w || (m.len() > 4 && w.len() >= 3 && m.starts_with(w)))?;
    Some(u32::try_from(i).ok()? + 1)
}

pub fn parse(words: &[&str]) -> Option<Date> {
    let words = match words {
        ["on" | "this" | "for", rest @ ..] => rest,
        w => w,
    };
    let weekday = |w: &str| {
        WEEKDAYS
            .iter()
            .position(|d| *d == w)
            .and_then(|i| u32::try_from(i).ok())
    };
    match words {
        [w] => weekday(w).map(|day| Date::Weekday { day, next: false }),
        ["next", w] => weekday(w).map(|day| Date::Weekday { day, next: true }),
        // "march 1st", "march the first"
        [m, "the", d @ ..] | [m, d @ ..] if month(m).is_some() => Some(Date::Month {
            month: month(m)?,
            day: day_of_month(d)?,
        }),
        // "the 1st of march", "1st of march", "the 15th"
        _ => {
            let rest = words.strip_prefix(&["the"]).unwrap_or(words);
            match rest.iter().position(|w| *w == "of") {
                Some(of) => {
                    let [m] = &rest[of + 1..] else {
                        return None;
                    };
                    Some(Date::Month {
                        month: month(m)?,
                        day: day_of_month(&rest[..of])?,
                    })
                }
                None if rest.len() < words.len() => day_of_month(rest).map(Date::OfMonth),
                None => None,
            }
        }
    }
}

/// Days from `today` (days since 1970) until the date's next occurrence,
/// today included. `None` for dates that don't exist.
pub fn days_ahead(date: Date, today: i64) -> Option<u32> {
    let (y, m, _) = civil(today);
    let target = match date {
        Date::Weekday { day, next } => {
            let ahead = (i64::from(day) - i64::from(weekday(today))).rem_euclid(7);
            today + ahead + if next && ahead == 0 { 7 } else { 0 }
        }
        Date::Month { month, day } => [y, y + 1]
            .into_iter()
            .filter_map(|y| real(y, month, day))
            .find(|n| *n >= today)?,
        Date::OfMonth(day) => {
            let next = if m == 12 { (y + 1, 1) } else { (y, m + 1) };
            [(y, m), next]
                .into_iter()
                .filter_map(|(y, m)| real(y, m, day))
                .find(|n| *n >= today)?
        }
    };
    u32::try_from(target - today).ok()
}

/// The day number, if the date exists ("february 30" does not).
fn real(y: i32, m: u32, d: u32) -> Option<i64> {
    let n = day_number(y, m, d);
    (civil(n) == (y, m, d)).then_some(n)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(t: &str) -> Option<Date> {
        parse(&t.split_whitespace().collect::<Vec<_>>())
    }

    #[test]
    fn phrasings() {
        let fri = Date::Weekday {
            day: 5,
            next: false,
        };
        let march1 = Date::Month { month: 3, day: 1 };
        for (t, want) in [
            ("friday", fri),
            ("on friday", fri),
            ("this friday", fri),
            ("next friday", Date::Weekday { day: 5, next: true }),
            ("march 1", march1),
            ("march 1st", march1),
            ("march 1 st", march1),
            ("the 1 st of march", march1),
            ("march first", march1),
            ("march the first", march1),
            ("the first of march", march1),
            ("1st of march", march1),
            ("on march 1st", march1),
            ("april 15th", Date::Month { month: 4, day: 15 }),
            ("april twenty first", Date::Month { month: 4, day: 21 }),
            ("may 2", Date::Month { month: 5, day: 2 }),
            ("sept 3", Date::Month { month: 9, day: 3 }),
            ("the 1st", Date::OfMonth(1)),
            ("the fifteenth", Date::OfMonth(15)),
        ] {
            assert_eq!(p(t), Some(want), "{t:?}");
        }
        for t in [
            "the", "march", "march 32", "may i", "the ball", "next", "1st",
        ] {
            assert_eq!(p(t), None, "{t:?}");
        }
    }

    #[test]
    fn resolved_from_today() {
        // Wednesday, 2025-10-22.
        let today = day_number(2025, 10, 22);
        let ahead = |t: &str| days_ahead(p(t).expect(t), today);
        assert_eq!(ahead("wednesday"), Some(0));
        assert_eq!(ahead("next wednesday"), Some(7));
        assert_eq!(ahead("friday"), Some(2));
        assert_eq!(ahead("next friday"), Some(2));
        assert_eq!(ahead("monday"), Some(5));
        assert_eq!(ahead("october 22"), Some(0));
        assert_eq!(ahead("october 23"), Some(1));
        // Passed this year, so next year's.
        assert_eq!(ahead("march 1"), Some(130));
        assert_eq!(ahead("the 25th"), Some(3));
        assert_eq!(ahead("the 1st"), Some(10));
        assert_eq!(ahead("february 30"), None);
    }
}
