//! Public holidays in Bavaria: fixed dates plus the ones that move with
//! Easter.

/// Days since 1970-01-01 (Howard Hinnant's `days_from_civil`).
pub fn day_number(y: i32, m: u32, d: u32) -> i64 {
    let y = i64::from(y) - i64::from(m <= 2);
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let m = i64::from(m);
    let doy = (153 * (m + if m > 2 { -3 } else { 9 }) + 2) / 5 + i64::from(d) - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

/// (year, month, day) of a day number.
pub fn civil(n: i64) -> (i32, u32, u32) {
    let z = n + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = yoe + era * 400 + i64::from(m <= 2);
    (
        i32::try_from(y).unwrap_or(0),
        u32::try_from(m).unwrap_or(1),
        u32::try_from(d).unwrap_or(1),
    )
}

/// 0 = Sunday, like `tm_wday`.
pub fn weekday(n: i64) -> u32 {
    u32::try_from((n + 4).rem_euclid(7)).unwrap_or(0)
}

/// Easter Sunday (anonymous Gregorian computus).
fn easter(y: i32) -> i64 {
    let a = y % 19;
    let (b, c) = (y / 100, y % 100);
    let (d, e) = (b / 4, b % 4);
    let f = (b + 8) / 25;
    let g = (b - f + 1) / 3;
    let h = (19 * a + b - d - g + 15) % 30;
    let (i, k) = (c / 4, c % 4);
    let l = (32 + 2 * e + 2 * i - h - k) % 7;
    let m = (a + 11 * h + 22 * l) / 451;
    let month = (h + l - 7 * m + 114) / 31;
    let day = (h + l - 7 * m + 114) % 31 + 1;
    day_number(
        y,
        u32::try_from(month).unwrap_or(4),
        u32::try_from(day).unwrap_or(1),
    )
}

fn year(y: i32) -> Vec<(i64, &'static str)> {
    let e = easter(y);
    let mut v = vec![
        (day_number(y, 1, 1), "New Year's Day"),
        (day_number(y, 1, 6), "Epiphany"),
        (e - 2, "Good Friday"),
        (e + 1, "Easter Monday"),
        (day_number(y, 5, 1), "Labour Day"),
        (e + 39, "Ascension Day"),
        (e + 50, "Whit Monday"),
        (e + 60, "Corpus Christi"),
        (day_number(y, 8, 15), "Assumption Day"),
        (day_number(y, 10, 3), "German Unity Day"),
        (day_number(y, 11, 1), "All Saints' Day"),
        (day_number(y, 12, 25), "Christmas Day"),
        (day_number(y, 12, 26), "Boxing Day"),
    ];
    v.sort_unstable();
    v
}

/// The first holiday after `today` (a day number), and its day number.
pub fn next(today: i64) -> (i64, &'static str) {
    let (y, _, _) = civil(today);
    year(y)
        .into_iter()
        .chain(year(y + 1))
        .find(|(n, _)| *n > today)
        .unwrap_or((today, "nothing"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn calendar_maths() {
        assert_eq!(day_number(1970, 1, 1), 0);
        assert_eq!(civil(day_number(2024, 2, 29)), (2024, 2, 29));
        // 2026-10-25 is a Sunday.
        assert_eq!(weekday(day_number(2026, 10, 25)), 0);
        assert_eq!(civil(easter(2024)), (2024, 3, 31));
        assert_eq!(civil(easter(2025)), (2025, 4, 20));
        assert_eq!(civil(easter(2026)), (2026, 4, 5));
    }

    #[test]
    fn next_holiday() {
        let at = |y, m, d| {
            let (n, name) = next(day_number(y, m, d));
            (civil(n), name)
        };
        assert_eq!(at(2026, 10, 25), ((2026, 11, 1), "All Saints' Day"));
        assert_eq!(at(2026, 12, 26), ((2027, 1, 1), "New Year's Day"));
        assert_eq!(at(2025, 6, 1), ((2025, 6, 9), "Whit Monday"));
        assert_eq!(at(2025, 6, 10), ((2025, 6, 19), "Corpus Christi"));
    }
}
