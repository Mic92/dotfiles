//! Spoken durations: "5 minutes", "an hour and a half", "1:30", "two minutes thirty".

const ONES: [&str; 20] = [
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen",
];
const TENS: [&str; 8] = [
    "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety",
];

/// A single number word or digit string.
pub fn number(w: &str) -> Option<u32> {
    if let Ok(v) = w.parse() {
        return Some(v);
    }
    let pos = |list: &[&str]| {
        list.iter()
            .position(|&o| o == w)
            .and_then(|i| u32::try_from(i).ok())
    };
    pos(&ONES).or_else(|| pos(&TENS).map(|i| (i + 2) * 10))
}

/// A spoken number of one or more words: "42", "twenty five",
/// "one hundred and five", "two thousand", "minus three", "3.5".
pub fn number_words(words: &[&str]) -> Option<f64> {
    let (sign, words) = match words {
        ["minus" | "negative", rest @ ..] => (-1.0, rest),
        _ => (1.0, words),
    };
    let (mut total, mut current, mut seen) = (0.0, 0.0, false);
    for (i, w) in words.iter().enumerate() {
        if let Ok(v) = w.parse::<f64>() {
            current += v;
        } else if let Some(v) = number(w) {
            current += f64::from(v);
        } else {
            match *w {
                "hundred" => {
                    current = current.max(1.0) * 100.0;
                    seen = true;
                }
                "thousand" => {
                    total += current.max(1.0) * 1000.0;
                    current = 0.0;
                    seen = true;
                }
                "a" if i == 0 => {}
                "and" if seen => {}
                _ => return None,
            }
            continue;
        }
        seen = true;
    }
    seen.then_some(sign * (total + current))
}

fn unit_seconds(w: &str) -> Option<f64> {
    match w {
        "hour" | "hours" | "hr" | "hrs" | "h" => Some(3600.0),
        "minute" | "minutes" | "min" | "mins" | "m" => Some(60.0),
        "second" | "seconds" | "sec" | "secs" | "s" => Some(1.0),
        _ => None,
    }
}

/// "1:30" (m:ss) or "1:30:00" (h:mm:ss).
fn colon(w: &str) -> Option<f64> {
    let parts: Vec<u32> = w
        .split(':')
        .map(|p| p.parse().ok())
        .collect::<Option<_>>()?;
    let (h, m, s) = match parts[..] {
        [m, s] => (0, m, s),
        [h, m, s] => (h, m, s),
        _ => return None,
    };
    (m < 60 || parts.len() == 2)
        .then_some(s < 60)?
        .then(|| f64::from(h * 3600 + m * 60 + s))
}

fn fraction(w: &str) -> Option<f64> {
    match w {
        "half" => Some(0.5),
        "quarter" | "quarters" => Some(0.25),
        _ => None,
    }
}

fn is_word(w: &str) -> bool {
    w.parse::<f64>().is_ok()
        || number(w).is_some()
        || unit_seconds(w).is_some()
        || fraction(w).is_some()
        || colon(w).is_some()
        || matches!(w, "a" | "an" | "and" | "of" | "hundred" | "couple" | "few")
}

fn smaller_unit(unit: f64) -> f64 {
    if unit >= 3600.0 { 60.0 } else { 1.0 }
}

/// Parse a spoken duration. A bare number counts as minutes, a trailing
/// number as the next smaller unit ("1 hour 20").
pub fn parse(words: &[&str]) -> Option<u32> {
    let mut total = 0.0;
    let mut num: Option<f64> = None;
    let mut fraction_only = false;
    let mut last_unit: Option<f64> = None;
    for (i, &tok) in words.iter().enumerate() {
        let next_is_unit = words.get(i + 1).and_then(|t| unit_seconds(t)).is_some();
        if let Some(unit) = unit_seconds(tok) {
            // Single letters are only units right after a number ("5 m").
            if tok.len() == 1 && num.is_none() {
                return None;
            }
            total += num.unwrap_or(1.0) * unit;
            last_unit = Some(unit);
            num = None;
            fraction_only = false;
        } else if let Some(secs) = colon(tok) {
            total += secs;
        } else if let Ok(v) = tok.parse::<f64>() {
            num = Some(num.unwrap_or(0.0) + v);
        } else if let Some(v) = number(tok) {
            num = Some(num.unwrap_or(0.0) + f64::from(v));
        } else if tok == "hundred" {
            num = Some(num.unwrap_or(1.0) * 100.0);
        } else if let Some(f) = fraction(tok) {
            // "three quarters" multiplies, "one and a half" adds.
            num = Some(match num {
                Some(n) if tok == "quarters" => n * f,
                Some(n) => n + f,
                None => f,
            });
            fraction_only = fraction_only || last_unit.is_some() || num == Some(f);
        } else if tok == "couple" {
            num = Some(2.0);
        } else if tok == "few" {
            num = Some(3.0);
        } else if (tok == "a" || tok == "an") && next_is_unit && num.is_none() {
            num = Some(1.0);
        }
    }
    if let Some(n) = num {
        total += match last_unit {
            None => n * 60.0,
            Some(u) if fraction_only => n * u,
            Some(u) => n * smaller_unit(u),
        };
    }
    let secs = total.round();
    // Range-checked first, so the cast cannot truncate or lose the sign.
    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    (1.0..=24.0 * 3600.0).contains(&secs).then_some(secs as u32)
}

/// Slot parser: every word must belong to the duration and the slot must not
/// stop short of one ("30" in "30 hours"), so it cannot swallow or split
/// neighbouring words.
pub fn slot(words: &[&str], next: Option<&str>) -> Option<u32> {
    if !words.iter().all(|w| is_word(w))
        || next.is_some_and(|n| is_word(n) && n != "a" && n != "and")
    {
        return None;
    }
    parse(words)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn secs(t: &str) -> Option<u32> {
        let words: Vec<&str> = t.split_whitespace().collect();
        parse(&words)
    }

    #[test]
    fn digits() {
        assert_eq!(secs("5 minutes"), Some(300));
        assert_eq!(secs("10 seconds"), Some(10));
        assert_eq!(secs("1.5 hours"), Some(5400));
        assert_eq!(secs("2 hours and 15 minutes"), Some(8100));
        assert_eq!(secs("1 h 30 m"), Some(5400));
    }

    #[test]
    fn words() {
        assert_eq!(secs("five minutes"), Some(300));
        assert_eq!(secs("twenty five minutes"), Some(1500));
        assert_eq!(secs("a minute"), Some(60));
        assert_eq!(secs("an hour"), Some(3600));
        assert_eq!(secs("one hundred and twenty seconds"), Some(120));
        assert_eq!(secs("a couple of minutes"), Some(120));
    }

    #[test]
    fn fractions() {
        assert_eq!(secs("half an hour"), Some(1800));
        assert_eq!(secs("an hour and a half"), Some(5400));
        assert_eq!(secs("one and a half minutes"), Some(90));
        assert_eq!(secs("two and a half hours"), Some(9000));
        assert_eq!(secs("a minute and a half"), Some(90));
        assert_eq!(secs("half a minute"), Some(30));
        assert_eq!(secs("a quarter of an hour"), Some(900));
        assert_eq!(secs("three quarters of an hour"), Some(2700));
        assert_eq!(secs("an hour and a quarter"), Some(4500));
    }

    #[test]
    fn trailing_number_uses_next_unit() {
        assert_eq!(secs("two minutes thirty"), Some(150));
        assert_eq!(secs("1 hour 20"), Some(4800));
        assert_eq!(secs("7"), Some(420));
    }

    #[test]
    fn numbers() {
        let n = |t: &str| number_words(&t.split_whitespace().collect::<Vec<_>>());
        assert_eq!(n("42"), Some(42.0));
        assert_eq!(n("twenty five"), Some(25.0));
        assert_eq!(n("one hundred and five"), Some(105.0));
        assert_eq!(n("a hundred"), Some(100.0));
        assert_eq!(n("two thousand three hundred"), Some(2300.0));
        assert_eq!(n("minus three"), Some(-3.0));
        assert_eq!(n("3.5"), Some(3.5));
        assert_eq!(n("five apples"), None);
        assert_eq!(n(""), None);
    }

    #[test]
    fn clock_notation() {
        assert_eq!(secs("1:30"), Some(90));
        assert_eq!(secs("1:30:00"), Some(5400));
        assert_eq!(secs("1:75"), None);
    }

    #[test]
    fn out_of_range() {
        assert_eq!(secs(""), None);
        assert_eq!(secs("0 minutes"), None);
        assert_eq!(secs("30 hours"), None);
    }

    #[test]
    fn slot_is_strict() {
        assert_eq!(slot(&["5", "minutes"], None), Some(300));
        assert_eq!(slot(&["5", "minutes", "please"], None), None);
        assert_eq!(slot(&["30"], Some("hours")), None);
        assert_eq!(slot(&["5", "minutes"], Some("and")), Some(300));
    }
}
