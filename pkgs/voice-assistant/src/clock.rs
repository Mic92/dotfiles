//! The local date and time the brain reads sentences at.

use std::mem;
use std::ptr;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Clock {
    /// Seconds since local midnight.
    pub now: u32,
    /// Days since 1970-01-01.
    pub today: i64,
}

impl Clock {
    pub fn local() -> Self {
        // SAFETY: time(NULL) has no preconditions; localtime_r writes only into
        // the zeroed `tm` we own and returns null on failure, which we check.
        let (t, tm) = unsafe {
            let t = libc::time(ptr::null_mut());
            let mut tm: libc::tm = mem::zeroed();
            if libc::localtime_r(&raw const t, &raw mut tm).is_null() {
                return Clock {
                    now: 0,
                    today: t / 86400,
                };
            }
            (t, tm)
        };
        let local = t + tm.tm_gmtoff;
        Clock {
            now: u32::try_from(local.rem_euclid(86400)).unwrap_or(0),
            today: local.div_euclid(86400),
        }
    }

    /// Wednesday, 2025-10-22 at `now`, for tests.
    #[cfg(test)]
    pub const fn at(now: u32) -> Self {
        Clock { now, today: 20383 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_time_is_within_a_day() {
        let c = Clock::local();
        assert!(c.now < 86400);
        assert!(c.today > 20000);
    }
}
