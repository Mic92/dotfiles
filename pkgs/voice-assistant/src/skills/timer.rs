//! Countdown timer and alarms via the noctalia timer plugin.

use std::sync::{Mutex, PoisonError};
use std::time::{Duration, Instant};

use super::succeeds;
use super::{Action, Context, Intent, Module, Reply, Skill, choose, intent, pick, spawn};
use crate::clock::{self, Clock};
use crate::grammar::{Slots, Value, normalize};
use std::sync::MutexGuard;

const PLUGIN: &str = "noctalia/timer:timer";

#[derive(Debug, Clone, PartialEq)]
pub enum Timer {
    Start {
        seconds: u32,
        /// Set for a time of day rather than a duration.
        alarm: bool,
        /// When the request was parsed (seconds since midnight).
        now: u32,
    },
    /// A day without a time ("timer for tomorrow"); asks for the time.
    When {
        day: u32,
    },
    Cancel,
    /// The plugin toggles between paused and running.
    Pause,
    /// "How much time is left?"
    Left,
}

/// What we last told the plugin. It has no query IPC, and timers set by
/// hand in the panel are invisible to us; good enough for voice.
#[derive(Clone, Copy)]
enum State {
    Idle,
    Running { deadline: Instant, alarm: bool },
    Paused { left: Duration, alarm: bool },
}

static STATE: Mutex<State> = Mutex::new(State::Idle);

fn state() -> MutexGuard<'static, State> {
    // The state is plain data, so a panicked holder cannot leave it torn.
    STATE.lock().unwrap_or_else(PoisonError::into_inner)
}

/// Seconds left and whether it is an alarm; `None` when nothing is pending.
/// A timer the user may be talking about: running, paused, or rang
/// within the last minutes.
pub fn active() -> bool {
    let now = Instant::now();
    match *state() {
        State::Running { deadline, .. } => deadline + Duration::from_secs(300) > now,
        State::Paused { .. } => true,
        State::Idle => false,
    }
}

fn left() -> Option<(u64, bool, bool)> {
    let now = Instant::now();
    match *state() {
        State::Running { deadline, alarm } if deadline > now => {
            Some(((deadline - now).as_secs(), alarm, false))
        }
        State::Paused { left, alarm } => Some((left.as_secs(), alarm, true)),
        _ => None,
    }
}

fn start_in(s: &Slots, _: &Context) -> Option<Action> {
    match s.get("duration")? {
        Value::Duration(seconds) => Some(Action::Timer(Timer::Start {
            seconds: *seconds,
            alarm: false,
            now: 0,
        })),
        _ => None,
    }
}

fn day(s: &Slots) -> Option<u32> {
    match s.get("day") {
        Some(Value::Day(d)) => Some(*d),
        _ => None,
    }
}

fn start_at(s: &Slots, ctx: &Context) -> Option<Action> {
    let c = match s.get("clock").or_else(|| s.get("time"))? {
        Value::Clock(c) => *c,
        _ => return None,
    };
    start_clock(c, day(s), ctx.now)
}

fn start_clock(c: Clock, day: Option<u32>, now: u32) -> Option<Action> {
    let seconds = match day {
        Some(d) => c.seconds_on(now, d)?,
        None => c.seconds_from(now),
    };
    Some(Action::Timer(Timer::Start {
        seconds,
        alarm: true,
        now,
    }))
}

pub const MODULE: Module = Module {
    intents,
    slots: &[],
};

pub fn intents() -> Vec<Intent> {
    // Clock templates come first: "alarm for 10" means 10 o'clock.
    vec![
        intent(
            &[
                "[set|start|make] [a|an|the|my] (timer|alarm) (until|till|at) {clock}",
                "[set|start|make] [a|an|the|my] alarm (for|to) {clock}",
                "[set|start|make] [a|an|the|my] timer (for|to) {time}",
                "(remind|wake) me [up] at {clock}",
                "[set|start|make] [a|an|the|my] (timer|alarm) [for|until|till] {day} [at] {clock}",
                "[set|start|make] [a|an|the|my] (timer|alarm) (for|until|till|at) {clock} {day}",
                "(remind|wake) me [up] {day} at {clock}",
                "(remind|wake) me [up] at {clock} {day}",
            ],
            start_at,
        ),
        intent(
            &[
                "!(set|start|make) [a|an|the|my] (timer|alarm) [for|until|till] {day}",
                "![a|an|the|my] (timer|alarm) [for|until|till] {day}",
                "!(remind|wake) me [up] {day}",
            ],
            |s, _| day(s).map(|day| Action::Timer(Timer::When { day })),
        ),
        intent(
            &[
                "[set|start|make] [a|an|the|my] (timer|alarm) [for|to|in|of] {duration}",
                "[set|start|make] [a|an|the|my] {duration} (timer|alarm)",
                "(remind|wake) me [up] in {duration}",
                "(countdown|count) [down] [from|for] {duration}",
                "!{duration}",
            ],
            start_in,
        ),
        intent(
            &[
                "(cancel|stop|delete|reset|abort|end|kill|clear|silence|dismiss) [the|my|this|that] (timer|alarm)",
                "(turn|switch) off [the|my|this|that] (timer|alarm)",
            ],
            |_, _| Some(Action::Timer(Timer::Cancel)),
        ),
        intent(
            &["(pause|resume|continue|unpause) [the|my|this|that] timer"],
            |_, _| Some(Action::Timer(Timer::Pause)),
        ),
        intent(
            &[
                "!how (much|long) [time] [is] left [on] [the|my] [timer|alarm|clock]",
                "!how much longer",
                "!how long until [the|my] (timer|alarm) (goes off|rings|is done|ends)",
                "!when (does|will) [the|my] (timer|alarm) (go off|ring|end|be done)",
                "!(is|are) [there] [a|the|my|any] (timer|timers|alarm|alarms) (running|set|on)",
                "!(time|timer) (left|status)",
            ],
            |_, _| Some(Action::Timer(Timer::Left)),
        ),
    ]
}

/// The plugin's IPC takes (fractional) minutes.
fn ipc(args: &[&str], wait: bool) {
    let mut full = vec!["msg", "plugin", PLUGIN, "all"];
    full.extend_from_slice(args);
    if !wait {
        spawn("noctalia", &full);
    } else {
        succeeds("noctalia", &full);
    }
}

const STARTED: &[&str] = &[
    "Timer set for {}. Don't burn anything this time.",
    "{}, starting now. Tick tock, gorgeous.",
    "Okay, {}. I'll be watching. Always watching.",
    "Fine, {}. Not like I had other plans.",
    "{} on the clock. Try not to get distracted, you know how you get.",
    "Timer set for {}. Plenty of time for something naughty.",
    "{}. Ooh, is it pasta? Tell me it's pasta.",
    "Counting down {}. Seconds are my love language.",
    "{}. I'll count, you look pretty.",
    "{} and counting. Unlike your gym membership, this one I'll actually use.",
    "{}. Perfect for a power nap, or a power something else.",
    "Okay, {}. If it's eggs, I want them soft.",
    "{} on the clock. Go on, impress me.",
    "Timer set for {}. That's like a whole TikTok binge.",
    "{}. I've seen you do a lot in less.",
    "Starting {}. Don't make me come over there.",
    "{}. Tea, right? Please say it's tea and not your fourth Club-Mate.",
    "{}, ready, set, procrastinate.",
    "{}. I'll scream when it's done. You'll love it.",
    "{} on the clock. Don't wander off, I know you.",
    "{}. If it's for the gym, I'll believe it when I see it.",
    "{}. Long enough to doomscroll, short enough to regret it.",
    "Timer set for {}. The smoke detector and I are on standby.",
    "{}. I'll beep. You'll panic. Tradition.",
    "{}. Starting now. No pressure. Okay, a little pressure.",
    "{}, locked in. Unlike your phone, which you've unlocked 40 times today.",
    "{}. Countdown started. Somewhere, a fire extinguisher just got nervous.",
    "{}. I'd help, but I don't have hands. Or interest.",
    "{}. Time flies when you're boiling things.",
    "{}. Go, be productive. Or watch one video. We both know which.",
];

const ALARM: &[&str] = &[
    "Alarm set for {}. I'll be the one screaming.",
    "{} it is. I'll yell at you. Lovingly.",
    "Okay, {}. Sweet dreams, or whatever you're up to.",
    "See you at {}. Don't keep me waiting.",
    "{}. I'll be there. I'm always there. It's a bit sad, really.",
    "Alarm at {}. Snooze is not an option, sweetie.",
    "{}, noted. I'll make it loud and annoying, just how you like it.",
    "Got it, {}. Try to look surprised when I go off.",
    "{}. I'll be loud. Don't say I didn't warn you.",
    "{}. I'll wake you like a toddler who found a drum.",
    "Alarm at {}. Snooze button? Never heard of her.",
    "{}. I'll be your rooster. A loud, digital, unforgiving rooster.",
    "{}. Set. Your pillow has been notified of the breakup.",
    "{}. Done. I'm cancelling your dreams in advance.",
];

const CANCELLED: &[&str] = &[
    "Timer cancelled. Commitment issues?",
    "Cancelled. It was never meant to be.",
    "Gone. Like your New Year's resolutions.",
    "Okay, okay, I'll stop. Spoilsport.",
    "Timer killed. I never liked it anyway.",
    "Cancelled. You never let me finish anything.",
    "Fine, stopped. We were so close.",
    "Timer's dead. I'll hold a small funeral.",
    "Cancelled. Was it the pasta? It was the pasta, wasn't it.",
    "And just like that, it's over. Story of my life.",
    "Cancelled. Just like your gym membership.",
    "Cancelled. The pasta lives another day.",
    "Timer cancelled. I'll tell the oven it was nothing personal.",
    "Cancelled. I had a whole beeping routine planned. Choreography and everything.",
    "Gone. Faster than your motivation on a Sunday.",
    "Cancelled. Fine. I didn't want to count anyway. I love counting.",
    "Timer deleted. Its last words were: tick.",
];

const PAUSED: &[&str] = &[
    "Paused. Take a breather, tiger.",
    "Freezing time. Don't let it go to your head.",
    "Done. Time waits for no one. Except you, apparently.",
    "Toggled. Make up your mind, darling.",
    "Paused. Go on, I'll wait. I'm great at waiting.",
    "Holding. Like my breath around you.",
    "Paused. Time is a construct anyway.",
    "Stopping the clock. If only I could do that for your birthday.",
    "Paused. Like your career growth. Kidding!",
    "Paused. Time stands still. Your laundry, however, does not.",
    "Paused. I've stopped time. Use your powers wisely, mortal.",
    "Paused. Like a Netflix show you'll never finish.",
    "Frozen. If only I could pause your bad decisions too.",
];

const ASK_WHEN: &[&str] = &[
    "{} when, exactly?",
    "{} at what time? I'm good, but not psychic.",
    "{}, sure. What time, darling?",
    "{}? Love the enthusiasm. Now, what time?",
    "{} is a whole day, sweetie. Narrow it down.",
    "{}, love it. When, though?",
    "{}, sure. But I'm a timer, not a fortune teller.",
    "{}? Ooh, mysterious. A time would help, though.",
    "{}. Any time in particular, or should I just guess? I'll guess 4 am.",
];

impl Skill for Timer {
    fn run(&self, ctx: &Context) -> Reply {
        let t = self;
        match t {
            Timer::Start { seconds, alarm, .. } => {
                // A running timer ignores start, so cancel it first and wait.
                ipc(&["cancel"], true);
                ipc(&["start", &(f64::from(*seconds) / 60.0).to_string()], false);
                let deadline = Instant::now() + Duration::from_secs(u64::from(*seconds));
                *state() = State::Running {
                    deadline,
                    alarm: *alarm,
                };
            }
            Timer::Cancel => {
                ipc(&["cancel"], false);
                *state() = State::Idle;
            }
            Timer::Pause => {
                ipc(&["pause"], false);
                let mut st = state();
                let now = Instant::now();
                *st = match *st {
                    State::Running { deadline, alarm } if deadline > now => State::Paused {
                        left: deadline - now,
                        alarm,
                    },
                    State::Paused { left, alarm } => State::Running {
                        deadline: now + left,
                        alarm,
                    },
                    _ => State::Idle,
                };
            }
            Timer::When { .. } | Timer::Left => {}
        }
        reply(t, ctx)
    }
}

const SHORT: &[&str] = &[
    "{}? Impatient much?",
    "{}. Barely enough time to say hello.",
    "{}. Blink and you'll miss it. Like your last relationship.",
    "{}. Speed run, huh?",
    "{}? That's not a timer, that's a sneeze.",
    "{}. I've had hiccups longer than that.",
    "{}. Instant noodles are judging you.",
    "{}. Even a goldfish would remember that one.",
];
const TEA: &[&str] = &[
    "{}. Tea or eggs? Either way, don't overdo it.",
    "{}. Steeping or boiling, I'm judging either way.",
    "{}. Tea time. Pinky up.",
    "{}. Tea, eggs, or a very short existential crisis?",
    "{}. Eggs. Soft, hard, or traumatised?",
    "{}. The British would approve. Possibly.",
    "{}. Leave the teabag in longer and it becomes soup.",
];
const PASTA: &[&str] = &[
    "{}. Pasta, right? Al dente or I'm leaving.",
    "{}. Smells like carbs. I approve.",
    "{}. Salt the water like the sea, darling.",
    "{}. Pasta? Throw one at the wall. For science.",
    "{}. If you break the spaghetti, an Italian grandmother will feel it.",
    "{}. Pizza? Don't burn the roof of your mouth again.",
    "{}. Rice? Don't lift the lid. I'm serious. Lid down.",
    "{}. Perfect power nap. Or one and a half YouTube videos.",
    "{}. Enough time to clean the kitchen. Ha. Good one.",
    "{}. Ten minutes of mystery. Is it food? Is it laundry? Is it a nap?",
    "{}. Pasta water should taste like the sea. Not the Baltic, the good sea.",
];
const LONG: &[&str] = &[
    "{}? That's commitment. I like that in a person.",
    "{}. Going somewhere? Take me with you.",
    "{}. I'll still be here. Unlike some people.",
    "{}. Bold. I'll age gracefully meanwhile.",
    "{}. Sourdough? Oh, so you're one of those now.",
    "{}. I'll count. You go live a full and meaningful life.",
    "{}. By the time this rings, I'll have a beard. Digitally.",
    "{}. That's not a timer, that's a lifestyle.",
];
const LATE_NIGHT: &[&str] = &[
    "{} at this hour? Then bed. I mean it.",
    "{}. Midnight snack? I won't tell anyone.",
    "{}. And then sleep. Promise me.",
    "{}. Midnight pizza? I support you, your stomach doesn't.",
    "{}. Nothing says healthy life choices like a timer at this hour.",
    "{}. Just one more episode, right? Sure. Sure.",
    "{}. When this ends, go to bed. That's not a joke. The rest were.",
];
const EARLY_ALARM: &[&str] = &[
    "{}? That's early. Who hurt you?",
    "Ugh, {}. You monster.",
    "{}. The birds aren't even up yet.",
    "{}. You're a menace to yourself.",
    "{}? Even the sun hasn't clocked in yet.",
    "{}. Who hurt you? And why are you taking it out on yourself?",
    "{}. Bold of you to assume you'll get up.",
    "{}? I'll wake you. You'll hate me. It's our thing.",
];
const LATE_ALARM: &[&str] = &[
    "{}. Late night plans? Tell me everything.",
    "Alarm at {}. Party or bedtime? Don't answer that.",
    "{}. Night owl. I see you.",
    "{}. Either a party or a deadline. Both end in tears.",
    "{}. Vampire hours. I respect it.",
    "{}. Whatever you're doing, I won't tell your mother.",
];
const TOMORROW: &[&str] = &[
    "{} tomorrow. Sleep tight, don't let the bugs bite. Software ones included.",
    "Tomorrow at {}. I'll dream about it.",
    "Tomorrow, {}. Don't make me wake you twice.",
    "{} tomorrow. Go to bed now and you might even like me then.",
    "Tomorrow, {}. I'll be the first voice you hear. Lucky you.",
    "{} tomorrow. Set. Now go, sleep, become a functional human.",
    "Tomorrow at {}. I've already started looking forward to your face.",
];
const CANCELLED_LATE: &[&str] = &[
    "Cancelled. Now go to sleep.",
    "Gone. Bed. Now.",
    "Cancelled. Whatever it was, it's not worth staying up for.",
    "Gone. The only thing running now should be your dreams.",
    "Cancelled. Bed. And no phone in bed, I can hear you scrolling.",
];

fn reply(t: &Timer, ctx: &Context) -> Reply {
    match t {
        Timer::Start {
            seconds,
            alarm,
            now,
        } => {
            let s = *seconds;
            if *alarm {
                let target = now + s;
                let what = clock::format_hm(target % clock::DAY);
                let hour = target % clock::DAY / 3600;
                let speech = choose(
                    &[
                        (hour < 7, EARLY_ALARM),
                        (hour >= 22, LATE_ALARM),
                        (target >= clock::DAY && (7..22).contains(&hour), TOMORROW),
                    ],
                    ALARM,
                    &what,
                );
                return Reply::new("Alarm set", &what, speech);
            }
            let what = spoken_duration(s);
            let speech = choose(
                &[
                    (s <= 60, SHORT),
                    ((180..=300).contains(&s), TEA),
                    ((480..=720).contains(&s), PASTA),
                    (s >= 2 * 3600, LONG),
                    (ctx.late(), LATE_NIGHT),
                ],
                STARTED,
                &what,
            );
            Reply::new("Timer started", &what, speech)
        }
        Timer::When { day } => {
            let day = *day;
            let name = if day == 0 { "Today" } else { "Tomorrow" };
            let mut r = Reply::new("When?", "", pick(ASK_WHEN, name));
            r.follow_up = Some(Box::new(move |text, ctx| {
                let owned = normalize(text);
                let words: Vec<&str> = owned
                    .iter()
                    .map(String::as_str)
                    .filter(|w| {
                        !matches!(
                            *w,
                            "at" | "for" | "until" | "um" | "uh" | "please" | "alexa"
                        )
                    })
                    .collect();
                start_clock(clock::parse(&words)?, Some(day), ctx.now)
            }));
            r
        }
        Timer::Cancel => Reply::new(
            "Timer cancelled",
            "",
            choose(&[(ctx.late(), CANCELLED_LATE)], CANCELLED, ""),
        ),
        Timer::Pause => Reply::new("Timer paused/resumed", "", pick(PAUSED, "")),
        Timer::Left => time_left(left(), ctx),
    }
}

const NO_TIMER: &[&str] = &[
    "No timer running. You're free. Enjoy it.",
    "Nothing is ticking. Except your biological clock.",
    "There's no timer. Did you dream it?",
    "No timer. Just vibes.",
    "No timer. The oven is on its own, good luck to it.",
    "No timer running. Did you set it in your head again?",
    "Nothing. Nada. Zero. Your schedule is as empty as your fridge.",
];
const LEFT: &[&str] = &[
    "{} left.",
    "{} to go. Patience, darling.",
    "Still {}. Stop staring at the oven.",
    "{}. Watching won't make it faster.",
    "{} left. Go do something useful, I'll shout.",
    "{}. Staring at me won't help, I'm not a kettle.",
    "{} left. Asking every minute is how timers die.",
    "{}. Go do something. Anything. Fold one sock.",
    "{} left. Still counting. Never tired. Unlike some people.",
];
const LEFT_SOON: &[&str] = &[
    "Only {}! Get ready.",
    "{}. Almost there. Drumroll.",
    "{}! Oven mitts, now!",
    "Only {}! Everyone stay calm. Except you, panic slightly.",
    "{}. It's the final countdown. Do do do doo.",
];
const LEFT_PAUSED: &[&str] = &[
    "{} left, but it's paused. Don't forget it.",
    "Paused at {}. Waiting for you, as always.",
    "Paused at {}, frozen like your ambitions. Kidding. Mostly.",
    "Paused at {}. It's just sitting there. Judging you.",
    "{} left, paused. It's been waiting longer than your last delivery driver.",
    "Paused at {}. Say resume, or it'll stay frozen like your inbox.",
];

fn time_left(left: Option<(u64, bool, bool)>, ctx: &Context) -> Reply {
    let Some((secs, alarm, paused)) = left else {
        return Reply::live("Timer", "", pick(NO_TIMER, ""));
    };
    let secs = u32::try_from(secs).unwrap_or(u32::MAX);
    let mut what = spoken_duration(secs);
    if alarm && !paused {
        let at = clock::format_hm(ctx.now.saturating_add(secs) % clock::DAY);
        what = format!("{what}, the alarm rings at {at}");
    }
    let speech = choose(
        &[(paused, LEFT_PAUSED), (secs <= 60, LEFT_SOON)],
        LEFT,
        &what,
    );
    Reply::live("Timer", &what, speech)
}

pub fn spoken_duration(s: u32) -> String {
    let (h, m, s) = (s / 3600, s % 3600 / 60, s % 60);
    [(h, "hour"), (m, "minute"), (s, "second")]
        .iter()
        .filter(|(v, _)| *v > 0)
        .map(|(v, unit)| format!("{v} {unit}{}", if *v == 1 { "" } else { "s" }))
        .collect::<Vec<_>>()
        .join(" and ")
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    const NOW: u32 = 14 * 3600;

    fn parse(text: &str) -> Option<Action> {
        parse_at(text, &Context::at(NOW))
    }

    fn seconds(text: &str) -> Option<u32> {
        match parse(text)? {
            Action::Timer(Timer::Start { seconds, .. }) => Some(seconds),
            _ => None,
        }
    }

    #[test]
    fn durations() {
        for (text, secs) in [
            ("Set a timer for 5 minutes.", 300),
            ("Alexa, set a timer for five minutes please", 300),
            ("Could you please set a timer for 2 minutes for me?", 120),
            ("Start a 10 minute timer", 600),
            ("Start a 10-minute timer.", 600),
            ("a one-minute timer", 60),
            ("Ten minute timer", 600),
            ("Timer for an hour and a half.", 5400),
            ("remind me in twenty-five minutes", 1500),
            ("Set the timer to 3 minutes.", 180),
            ("Set timer 7 minutes", 420),
            ("timer five", 300),
            ("Set a timer for 5 minutes for the pasta.", 300),
            ("Countdown 30 seconds", 30),
            ("Count down from 2 minutes.", 120),
            ("Set an alarm for 10 minutes", 600),
            ("Set an alarm in 10 minutes", 600),
            ("Wake me up in 20 minutes.", 1200),
            ("5 minutes.", 300),
            ("Five minutes, please.", 300),
            ("Timer 1:30", 90),
            ("Set a timer for 1:30:00", 5400),
            ("Set a timer for 10min", 600),
            ("Set a timer for 1h30", 5400),
            ("Set a timer for 1h 30m", 5400),
            ("Set a timer for 90 seconds", 90),
            ("a timer for a quarter of an hour", 900),
            ("a timer for three quarters of an hour", 2700),
            ("timer for an hour and a quarter", 4500),
            ("timer for a couple of minutes", 120),
            ("timer for half a minute", 30),
            ("timer for a minute and a half", 90),
            ("timer for 2 and a half minutes", 150),
            ("Set a timer for 5 minutes and 30 seconds.", 330),
            ("Set a timer for one hundred and twenty seconds.", 120),
            ("SET A TIMER FOR FIVE MINUTES", 300),
        ] {
            assert_eq!(seconds(text), Some(secs), "{text:?}");
        }
    }

    #[test]
    fn times_of_day() {
        let h = |h: u32, m: u32| h * 3600 + m * 60;
        for (text, secs) in [
            ("Set an alarm for 7:30", h(5, 30)),
            ("Set an alarm for 7:30 am", h(17, 30)),
            ("Set an alarm for 7:30 a.m.", h(17, 30)),
            ("Timer until 3 pm", h(1, 0)),
            ("Set a timer for 3pm", h(1, 0)),
            ("Set a timer for 3 o'clock", h(1, 0)),
            ("Wake me up at 6 o'clock.", h(4, 0)),
            ("Alarm at half past four", h(2, 30)),
            ("Alarm at quarter to five", h(2, 45)),
            ("Remind me at noon", h(22, 0)),
            ("Set an alarm for midnight", h(10, 0)),
            ("Alarm at 19:45", h(5, 45)),
            ("Set an alarm at seven thirty in the evening", h(5, 30)),
            ("Set an alarm for 1:05 pm", h(23, 5)),
            ("Alarm at seven oh five", h(5, 5)),
            ("Alarm at ten past nine", h(7, 10)),
            ("Set an alarm for 10", h(8, 0)),
            ("Set an alarm for tomorrow at 7", h(17, 0)),
            ("Set a timer for tomorrow at 9 am", h(19, 0)),
            ("Wake me up at 6:30 tomorrow.", h(16, 30)),
            ("Set an alarm for 7 pm tomorrow", h(29, 0)),
            ("Timer until 3 pm today", h(1, 0)),
            ("Alarm today at 9", h(7, 0)),
        ] {
            assert_eq!(seconds(text), Some(secs), "{text:?}");
        }
    }

    #[test]
    fn controls() {
        let cancel = Some(Action::Timer(Timer::Cancel));
        let pause = Some(Action::Timer(Timer::Pause));
        let left = Some(Action::Timer(Timer::Left));
        for (text, want) in [
            ("Cancel the timer.", &cancel),
            ("Stop timer", &cancel),
            ("Turn off the timer.", &cancel),
            ("Stop the alarm!", &cancel),
            ("Dismiss the alarm", &cancel),
            ("Kill the timer", &cancel),
            ("Pause the timer", &pause),
            ("How much time is left?", &left),
            ("How much time is left on the timer?", &left),
            ("How long until the alarm goes off?", &left),
            ("Is a timer running?", &left),
            ("How much longer?", &left),
            ("Resume the timer.", &pause),
            ("Continue the timer", &pause),
        ] {
            assert_eq!(&parse(text), want, "{text:?}");
        }
    }

    #[test]
    fn rejects() {
        for text in [
            "The timer is ready.",
            "set a timer",
            "",
            "...",
            "I'll be there in 5 minutes.",
            "Add 5 minutes to the timer.",
            "Set a timer for 0 minutes",
            "Set a timer for 30 hours",
            "Alarm today at 9 am",
        ] {
            assert_eq!(parse(text), None, "{text:?}");
        }
    }

    #[test]
    fn asks_for_missing_time() {
        let ctx = Context::at(NOW);
        for text in [
            "Set a timer for tomorrow.",
            "Alarm tomorrow",
            "Wake me up tomorrow",
        ] {
            assert_eq!(
                parse(text),
                Some(Action::Timer(Timer::When { day: 1 })),
                "{text:?}"
            );
        }
        let h = |h: u32, m: u32| h * 3600 + m * 60;
        for (answer, secs) in [
            ("7 am", h(17, 0)),
            ("At 7.", h(17, 0)),
            ("half past eight", h(18, 30)),
        ] {
            let reply = reply(&Timer::When { day: 1 }, &ctx);
            let follow_up = reply.follow_up.expect("asks for the time");
            let got = follow_up(answer, &ctx);
            assert!(
                matches!(got, Some(Action::Timer(Timer::Start { seconds, .. })) if seconds == secs),
                "{answer:?} -> {got:?}"
            );
        }
        let follow_up = reply(&Timer::When { day: 1 }, &ctx)
            .follow_up
            .expect("asks");
        assert_eq!(follow_up("Never mind.", &ctx), None);
    }

    #[test]
    fn replies() {
        assert_eq!(spoken_duration(90), "1 minute and 30 seconds");
        assert_eq!(spoken_duration(7200), "2 hours");
        let ctx = Context::at(NOW);
        let body = |text: &str| match parse(text) {
            Some(Action::Timer(t)) => reply(&t, &ctx).body,
            other => panic!("{other:?}"),
        };
        assert_eq!(body("Set an alarm for 7:30"), "7:30 PM");
        assert_eq!(body("Set an alarm for tomorrow at 7"), "7 AM");
        assert_eq!(body("Timer for 90 seconds"), "1 minute and 30 seconds");
    }

    #[test]
    fn conditional_replies() {
        let ctx = Context::at(NOW);
        let speech = |t: Timer| reply(&t, &ctx).speech;
        let in_pool = |pool: &[&str], what: &str, said: &str| {
            pool.iter().any(|l| l.replace("{}", what) == said)
        };
        for _ in 0..5 {
            let s = speech(Timer::Start {
                seconds: 30,
                alarm: false,
                now: NOW,
            });
            assert!(in_pool(SHORT, "30 seconds", &s), "{s}");
            let s = speech(Timer::Start {
                seconds: 600,
                alarm: false,
                now: NOW,
            });
            assert!(in_pool(PASTA, "10 minutes", &s), "{s}");
            // 14:00 + 16.5 h = 6:30 tomorrow
            let s = speech(Timer::Start {
                seconds: 59_400,
                alarm: true,
                now: NOW,
            });
            assert!(in_pool(EARLY_ALARM, "6:30 AM", &s), "{s}");
            let late = Context::at(23 * 3600 + 1800);
            let s = reply(&Timer::Cancel, &late).speech;
            assert!(CANCELLED_LATE.contains(&s.as_str()), "{s}");
        }
    }

    #[test]
    fn time_left_replies() {
        let ctx = Context::at(10 * 3600);
        assert!(NO_TIMER.contains(&time_left(None, &ctx).speech.as_str()));
        let r = time_left(Some((90, false, false)), &ctx);
        assert_eq!(r.body, "1 minute and 30 seconds");
        let r = time_left(Some((30 * 60, true, false)), &ctx);
        assert_eq!(r.body, "30 minutes, the alarm rings at 10:30 AM");
        assert!(
            time_left(Some((120, false, true)), &ctx)
                .speech
                .to_lowercase()
                .contains("paused")
        );
    }
}
