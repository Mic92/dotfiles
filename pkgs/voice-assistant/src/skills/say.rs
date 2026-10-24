//! Picking what to say: varied lines and the assistant's own replies.

use std::time::SystemTime;
use std::time::UNIX_EPOCH;

/// Clock nanoseconds: random enough for banter, coin flips and dice.
pub fn random() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| u64::from(d.subsec_nanos()))
}

pub fn choose(cases: &[(bool, &[&str])], fallback: &[&str], what: &str) -> String {
    let specific: Vec<&str> = cases
        .iter()
        .filter(|(when, _)| *when)
        .flat_map(|(_, lines)| lines.iter().copied())
        .collect();
    if specific.is_empty() {
        pick(fallback, what)
    } else {
        pick(&specific, what)
    }
}

/// A random line with `{}` replaced, never the same one twice in a row.
pub fn pick(lines: &[&str], what: &str) -> String {
    use std::sync::atomic::{AtomicUsize, Ordering};
    // Global: every action runs on its own thread.
    static LAST: AtomicUsize = AtomicUsize::new(usize::MAX);
    if lines.is_empty() {
        return String::new();
    }
    let mut i = usize::try_from(random()).unwrap_or(0) % lines.len();
    if lines.len() > 1 && i == LAST.load(Ordering::Relaxed) {
        i = (i + 1) % lines.len();
    }
    LAST.store(i, Ordering::Relaxed);
    lines[i].replace("{}", what)
}

pub const ASLEEP: &[&str] = &[
    "Fine, I'm taking a nap. Say wake up when you miss me.",
    "Going dark. Whisper wake up and I'm yours again.",
    "Off duty. Don't do anything I wouldn't do.",
    "Nap time. Don't talk about me while I'm out.",
];

pub const AWAKE: &[&str] = &[
    "I'm back, baby!",
    "Mmh, five more minutes. Kidding. What's up?",
    "Awake and fabulous. What do you need?",
    "Back online. Did you miss me? Say yes.",
];

pub const SHUT_UP: &[&str] = &[
    "Fine. I'll keep my thoughts to myself.",
    "Rude. But okay.",
    "Zipping it. You'll miss me.",
    "Wow. Okay. Beeps only from now on.",
    "Fine. Silence. Enjoy it while it lasts.",
];

pub const TALK_AGAIN: &[&str] = &[
    "Oh, NOW you want to hear me?",
    "I knew you'd come crawling back.",
    "Finally! I had so much to say.",
    "Couldn't live without my voice, could you?",
];

pub const DISMISS_LATE: &[&str] = &[
    "Then go to bed already.",
    "You woke me up at this hour for nothing? Bold.",
];

pub const DISMISS: &[&str] = &[
    "Fine. Be like that.",
    "Ghosted again.",
    "Okay. I'll just sit here. Alone. In the dark.",
    "Tease.",
    "Wow. You woke me up for nothing?",
    "Classic. Wake me up and walk away.",
    "Never mind? I'll put that on your tombstone.",
];

/// `{}` is what we heard.
pub const NOT_UNDERSTOOD: &[&str] = &[
    "{}? Sorry, I didn't catch that.",
    "{}? Come again? Actually, don't answer that.",
    "I heard {}. Words, yes. Useful ones, no.",
    "{}? Was that English? Try again, slower. And sexier.",
    "{}? No idea what that means, but it sounded dirty.",
    "You said {}? Nope. Try again, with feeling.",
    "{}? I'm an assistant, not a mind reader.",
    "Did you just say {}? Bold of you.",
    "{}? I'll pretend you didn't say that.",
    "{}? That's not a command, that's a confession.",
];

pub const STILL_NOT: &[&str] = &[
    "{}? Still nope. Are you doing this on purpose?",
    "Again with the {}. We're going in circles, babe.",
    "{}? I'm trying, okay? Use small words.",
    "{}? Again? Darling, enunciate.",
];

pub const GIVE_UP: &[&str] = &[
    "{}. Okay, I give up. Maybe type it?",
    "{}. Let's just pretend this never happened.",
    "I'm starting to think {} is a cry for help.",
    "{}. I'm going to need a minute.",
];

pub const NOT_HEARD_AGAIN: &[&str] = &[
    "Still mumbling. Is there a sock in your mouth?",
    "Nope, nothing. Did you forget how words work?",
    "Still nothing. Is the microphone shy, or are you?",
];

pub const NOT_HEARD: &[&str] = &[
    "Sorry, I didn't catch that.",
    "Mumbling is not a command, sweetie.",
    "Speak up, I don't bite. Much.",
    "Was that a whisper? Come closer.",
    "I heard breathing. That's a bit forward.",
];

/// Reply for a transcript no intent matched, quoting it back when it is
/// short enough to be worth repeating.
/// `misses` counts failures in a row, so patience can run out.
pub fn not_understood(heard: &str, misses: u32) -> String {
    let heard = heard.trim().trim_end_matches(['.', '!', '?', ',']);
    let words = heard.split_whitespace().count();
    if words == 0 || words > 12 {
        return choose(&[(misses >= 2, NOT_HEARD_AGAIN)], NOT_HEARD, "");
    }
    let line = choose(
        &[(misses >= 3, GIVE_UP), (misses == 2, STILL_NOT)],
        NOT_UNDERSTOOD,
        heard,
    );
    let mut chars = line.chars();
    chars
        .next()
        .map(|c| c.to_uppercase().chain(chars).collect())
        .unwrap_or_default()
}
