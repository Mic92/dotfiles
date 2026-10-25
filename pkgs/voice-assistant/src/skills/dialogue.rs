//! Words that mean nothing on their own ("stop", "nah", "again", "and
//! tomorrow?") are parsed once into dialogue acts, then handed to what the
//! user may be referring to, most salient first: the question we asked,
//! the last action, a timer, the music.

use std::sync::LazyLock;

use super::media::Media;
use super::timer::Timer;
use super::{Action, Context, parse_tier, pick, slot_parser};
use crate::grammar::{Slots, Template, Value, normalize};

#[derive(Debug, Clone, PartialEq)]
pub enum Act {
    Yes,
    /// Also "never mind", "forget it".
    No,
    /// "I don't know", "maybe".
    Maybe,
    /// "stop", "stop it".
    Stop,
    /// "cancel that", "abort".
    Cancel,
    Pause,
    Resume,
    /// "again", "another one".
    Again,
    /// "and tomorrow?": the last action with these words for one slot.
    Refine(Vec<String>),
}

/// What is going on besides a question.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Focus {
    /// The last action, if recent.
    pub previous: Option<Action>,
    /// A timer is running, paused, or rang a moment ago.
    pub timer: bool,
    /// Something was playing when we started listening.
    pub music: bool,
}

/// A question we asked and wait to have answered.
pub trait Question: Send {
    /// Yes, no and the like. Unhandled acts fall through.
    fn act(&self, _act: &Act) -> Option<Action> {
        None
    }

    /// Free answers: "the computer", "7 am".
    fn answer(&self, _text: &str, _ctx: &Context) -> Option<Action> {
        None
    }
}

/// "Shall I?": one action for yes, another for no.
pub struct Confirm {
    pub yes: Action,
    pub no: Action,
}

impl Question for Confirm {
    fn act(&self, act: &Act) -> Option<Action> {
        match act {
            Act::Yes => Some(self.yes.clone()),
            Act::No | Act::Cancel | Act::Stop => Some(self.no.clone()),
            _ => None,
        }
    }
}

type Build = fn(&Slots) -> Option<Act>;

struct Rule {
    templates: Vec<Template>,
    build: Build,
}

fn rule(templates: &[&str], build: Build) -> Rule {
    Rule {
        templates: templates
            .iter()
            .filter_map(|t| {
                Template::parse(t)
                    .inspect_err(|e| eprintln!("voice-assistant: {e}"))
                    .ok()
            })
            .collect(),
        build,
    }
}

const TEMPLATES: &[(&[&str], Build)] = &[
    (
        &[
            "!stop [it|that|this] [now|please|immediately|right now]",
            "![can you|could you|please] stop [it|that|this] [please]",
            "!stop just stop",
            "!stop (what you're doing|what you are doing|doing that|doing this)",
            "!(halt|quit|terminate) [it|that]",
        ],
        |_| Some(Act::Stop),
    ),
    (
        &[
            "![oops|actually|ai] (cancel|abort) [it|that|this] [now]",
            "![can you|could you|please] (cancel|abort) [it|that|this] [please]",
            "!(cancel|abort) [that|the|my] [last|previous|current] (thing|request|command|input|task|action)",
            "!(scrap|scratch) [it|that]",
            "!abort mission",
            "!command cancel",
            "!i repeat cancel",
        ],
        |_| Some(Act::Cancel),
    ),
    (&["!(pause|pause it|pause that)"], |_| Some(Act::Pause)),
    (
        &["!(resume|unpause|continue|resume it|unpause it|continue it)"],
        |_| Some(Act::Resume),
    ),
    (
        &[
            "!never mind [don't do that|cancel that]",
            "!nevermind [cancel that]",
            "![actually] forget (it|that|about it) [i don't need it|i do not need it anymore]",
            "!forget (my previous command|what i told you to do|what i said)",
            "!ignore (the|my) (previous|last) command",
            "!(don't|do not) do that",
            "![stop] i (changed|have changed|'ve changed) my mind",
            "!nothing",
        ],
        |_| Some(Act::No),
    ),
    (
        &[
            "![tell|give|say|do] [me|us] (again|once more|one more|one more time|another|another one|and another|do it again|same again|more|even more|more please|keep going|a bit more|bit more)",
        ],
        |_| Some(Act::Again),
    ),
    (
        &[
            "!(and|also|plus|what about|how about|and what about|and how about|same for|now|and now) {text}",
        ],
        |s| {
            let Value::Text(t) = s.get("text")? else {
                return None;
            };
            let all: Vec<&str> = t.split_whitespace().collect();
            // "and in munich", "what about at 3"
            let words = match all.as_slice() {
                ["in" | "at" | "for" | "on", rest @ ..] if !rest.is_empty() => rest,
                w => w,
            };
            Some(Act::Refine(words.iter().map(|w| (*w).to_owned()).collect()))
        },
    ),
];

static RULES: LazyLock<Vec<Rule>> =
    LazyLock::new(|| TEMPLATES.iter().map(|(t, b)| rule(t, *b)).collect());

/// The act a whole utterance is, if any.
pub fn act(words: &[&str]) -> Option<Act> {
    RULES
        .iter()
        .flat_map(|r| r.templates.iter().map(move |t| (r, t)))
        .filter_map(|(r, t)| {
            let (skipped, slots) = t.find(words, &slot_parser)?;
            Some((skipped, (r.build)(&slots)?))
        })
        .min_by_key(|(skipped, _)| *skipped)
        .map(|(_, a)| a)
}

/// Longer sentences are someone talking about something else.
const MAX_ANSWER_WORDS: usize = 6;

/// Yes, no or maybe as an answer to a question. Loose on purpose, so
/// only used while one is pending.
fn answer_word(words: &[&str]) -> Option<Act> {
    if words.len() > MAX_ANSWER_WORDS {
        return None;
    }
    let has = |list: &[&str]| words.iter().any(|w| list.contains(w));
    let pair = |list: &[[&str; 2]]| words.windows(2).any(|p| list.iter().any(|l| p == l));
    if has(&["maybe", "perhaps", "dunno", "idk", "possibly"])
        || pair(&[
            ["don't", "know"],
            ["not", "sure"],
            ["no", "idea"],
            ["no", "clue"],
        ])
    {
        Some(Act::Maybe)
    } else if has(&[
        "no", "nope", "nah", "naw", "nay", "cancel", "don't", "abort", "wait", "negative", "not",
        "pass",
    ]) {
        Some(Act::No)
    } else if has(&[
        "yes",
        "yeah",
        "yep",
        "yup",
        "yeap",
        "sure",
        "confirm",
        "confirmed",
        "absolutely",
        "please",
        "ok",
        "okay",
        "affirmative",
        "definitely",
        "certainly",
        "indeed",
        "correct",
        "agreed",
    ]) || pair(&[
        ["do", "it"],
        ["go", "ahead"],
        ["of", "course"],
        ["uh", "huh"],
    ]) {
        Some(Act::Yes)
    } else {
        None
    }
}

const NOT_AGAIN: &[&str] = &[
    "Again? Once was plenty, darling.",
    "No encores for that one. I have standards.",
    "Do it again? What am I, your intern?",
    "Encore? Pay me first.",
];

const LOST: &[&str] = &[
    "Another what? I've lost the thread, babe.",
    "Again? We haven't even started.",
    "More of what? Use your words.",
    "Another one of what? Help me out here, I'm only software.",
];

const UNSURE: &[&str] = &[
    "No rush. Ask me again when you've made up your mind.",
    "Take your time. I'll forget about it in the meantime.",
    "Indecisive. Cute. Let's drop it for now.",
];

/// An act without a question, resolved against the rest of the focus.
pub fn ambient(act: &Act, f: &Focus) -> Option<Action> {
    Some(match act {
        Act::Again => match &f.previous {
            None => Action::Say(pick(LOST, "")),
            Some(p) if p.skill().is_some_and(|s| s.repeatable()) => p.clone(),
            Some(_) => Action::Say(pick(NOT_AGAIN, "")),
        },
        Act::Refine(words) => {
            let words: Vec<&str> = words.iter().map(String::as_str).collect();
            return f.previous.as_ref()?.skill()?.refine(&words);
        }
        Act::Stop | Act::Cancel if f.timer => Action::Timer(Timer::Cancel),
        Act::Stop if f.music => Action::Media(Media::Stop),
        Act::Pause | Act::Resume if f.timer => Action::Timer(Timer::Pause),
        Act::Pause => Action::Media(Media::Pause),
        Act::Resume => Action::Media(Media::Play),
        // "never mind" never touches a timer or the music.
        Act::Stop | Act::Cancel | Act::No => Action::Dismiss,
        Act::Yes | Act::Maybe => return None,
    })
}

/// What the user meant: the question's answer first, then a full command,
/// then an act about whatever else is going on.
pub fn respond(text: &str, ctx: &Context, question: Option<&dyn Question>) -> Option<Action> {
    if let Some(q) = question {
        if let Some(a) = q.answer(text, ctx) {
            return Some(a);
        }
        let owned = normalize(text);
        let words: Vec<&str> = owned.iter().map(String::as_str).collect();
        if let Some(act) = act(&words).or_else(|| answer_word(&words)) {
            if let Some(a) = q.act(&act) {
                return Some(a);
            }
            match act {
                Act::Stop | Act::Cancel | Act::No => return Some(Action::Dismiss),
                Act::Maybe => return Some(Action::Say(pick(UNSURE, ""))),
                _ => {}
            }
        }
    }
    parse_tier(text, ctx, false)
}

#[cfg(test)]
mod tests {
    use super::super::parse_at;
    use super::*;

    fn with(focus: Focus) -> Context {
        Context {
            focus,
            ..Context::at(0)
        }
    }

    #[test]
    fn templates_parse() {
        for t in TEMPLATES.iter().flat_map(|(t, _)| t.iter()) {
            assert!(Template::parse(t).is_ok(), "{t:?}");
        }
    }

    #[test]
    fn ambient_acts() {
        let timer = Focus {
            timer: true,
            music: true,
            ..Focus::default()
        };
        let music = Focus {
            music: true,
            ..Focus::default()
        };
        let quiet = Focus::default();
        for (act, f, want) in [
            (Act::Stop, &timer, Some(Action::Timer(Timer::Cancel))),
            (Act::Cancel, &timer, Some(Action::Timer(Timer::Cancel))),
            (Act::No, &timer, Some(Action::Dismiss)),
            (Act::Stop, &music, Some(Action::Media(Media::Stop))),
            // Cancelling a request must not kill the music.
            (Act::Cancel, &music, Some(Action::Dismiss)),
            (Act::Stop, &quiet, Some(Action::Dismiss)),
            (Act::Pause, &timer, Some(Action::Timer(Timer::Pause))),
            (Act::Pause, &music, Some(Action::Media(Media::Pause))),
            (Act::Resume, &quiet, Some(Action::Media(Media::Play))),
            (Act::Yes, &quiet, None),
        ] {
            assert_eq!(ambient(&act, f), want, "{act:?} {f:?}");
        }
    }

    #[test]
    fn phrasings() {
        let ctx = with(Focus {
            timer: true,
            ..Focus::default()
        });
        let cancel = Some(Action::Timer(Timer::Cancel));
        let dismiss = Some(Action::Dismiss);
        for (text, want) in [
            ("Stop!", &cancel),
            ("stop it now!", &cancel),
            ("Cancel.", &cancel),
            ("Alexa, cancel.", &cancel),
            ("Cancel it, please.", &cancel),
            ("oops cancel that", &cancel),
            ("please cancel that last command", &cancel),
            ("abort mission", &cancel),
            ("Cancel the timer.", &cancel),
            ("Pause.", &Some(Action::Timer(Timer::Pause))),
            ("never mind", &dismiss),
            ("Alexa, forget it.", &dismiss),
            ("never mind, don't do that", &dismiss),
            ("actually forget that i don't need it", &dismiss),
            ("forget what i told you to do", &dismiss),
            ("stop, i changed my mind", &dismiss),
            ("Nothing.", &dismiss),
        ] {
            assert_eq!(&parse_at(text, &ctx), want, "{text:?}");
        }
        assert_eq!(parse_at("Stop!", &Context::at(0)), dismiss);
    }

    fn shut_down() -> Confirm {
        Confirm {
            yes: Action::Nap(1),
            no: Action::Say("phew".into()),
        }
    }

    #[test]
    fn questions_come_first() {
        let ctx = with(Focus {
            timer: true,
            music: true,
            ..Focus::default()
        });
        let q = shut_down();
        let ask = |t| respond(t, &ctx, Some(&q));
        let phew = Some(Action::Say("phew".into()));
        for t in ["Yes.", "Yeah, do it", "Do it!", "sure thing", "of course"] {
            assert_eq!(ask(t), Some(Action::Nap(1)), "{t:?}");
        }
        for t in [
            "No, wait!",
            "Please don't.",
            "nah",
            "forget it",
            "cancel that",
            "stop",
            "i'll pass",
        ] {
            assert_eq!(ask(t), phew, "{t:?}");
        }
        assert!(matches!(ask("i don't know"), Some(Action::Say(s)) if s != "phew"));
        // A full command ignores the question.
        assert_eq!(ask("stop the music"), Some(Action::Media(Media::Stop)));
        // Without a question, "stop" is about the timer.
        assert_eq!(
            respond("stop", &ctx, None),
            Some(Action::Timer(Timer::Cancel))
        );
        assert_eq!(respond("yes", &ctx, None), None);
    }

    #[test]
    fn unhandled_acts_dismiss_the_question() {
        struct Free;
        impl Question for Free {}
        let ctx = with(Focus {
            timer: true,
            ..Focus::default()
        });
        assert_eq!(respond("stop", &ctx, Some(&Free)), Some(Action::Dismiss));
    }

    #[test]
    fn answer_words() {
        let a = |t: &str| {
            let w = normalize(t);
            answer_word(&w.iter().map(String::as_str).collect::<Vec<_>>())
        };
        assert_eq!(a("What?"), None);
        assert_eq!(a("no idea"), Some(Act::Maybe));
        assert_eq!(
            a("well I was going to say something else entirely here"),
            None
        );
    }
}
