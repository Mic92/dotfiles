//! Bare "stop", "pause", "cancel that": what they mean depends on what is
//! going on, so they are resolved against a snapshot of the situation
//! rather than guessed by each skill.

use super::media::Media;
use super::timer::Timer;
use super::{Action, Intent, Module, intent};

/// What the user may be talking about when they don't say.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct Situation {
    /// We asked a question and wait for the answer.
    pub asking: bool,
    /// Something was playing when we started listening.
    pub music: bool,
    /// A timer is running, paused, or rang a moment ago.
    pub timer: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Bare {
    /// "stop", "stop it".
    Stop,
    /// "cancel that", "abort".
    Cancel,
    Pause,
    Resume,
}

pub fn resolve(word: Bare, s: &Situation) -> Action {
    match word {
        Bare::Stop | Bare::Cancel if s.asking => Action::Dismiss,
        Bare::Stop | Bare::Cancel if s.timer => Action::Timer(Timer::Cancel),
        Bare::Stop if s.music => Action::Media(Media::Stop),
        Bare::Stop | Bare::Cancel => Action::Dismiss,
        Bare::Pause | Bare::Resume if s.timer => Action::Timer(Timer::Pause),
        Bare::Pause => Action::Media(Media::Pause),
        Bare::Resume => Action::Media(Media::Play),
    }
}

pub const MODULE: Module = Module {
    intents,
    slots: &[],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            &[
                "!stop [it|that|this] [now|please|immediately|right now]",
                "![can you|could you|please] stop [it|that|this] [please]",
                "!stop just stop",
                "!stop (what you're doing|what you are doing|doing that|doing this)",
                "!(halt|quit|terminate) [it|that]",
            ],
            |_, ctx| Some(resolve(Bare::Stop, &ctx.situation)),
        ),
        intent(
            &[
                "![oops|actually|ai] (cancel|abort) [it|that|this] [now]",
                "![can you|could you|please] (cancel|abort) [it|that|this] [please]",
                "!(cancel|abort) [that|the|my] [last|previous|current] (thing|request|command|input|task|action)",
                "!(scrap|scratch) [it|that]",
                "!abort mission",
                "!command cancel",
                "!i repeat cancel",
            ],
            |_, ctx| Some(resolve(Bare::Cancel, &ctx.situation)),
        ),
        intent(&["!(pause|pause it|pause that)"], |_, ctx| {
            Some(resolve(Bare::Pause, &ctx.situation))
        }),
        intent(
            &["!(resume|unpause|continue|resume it|unpause it|continue it)"],
            |_, ctx| Some(resolve(Bare::Resume, &ctx.situation)),
        ),
        // Never a timer or the music: "never mind" while one runs keeps it.
        intent(
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
            |_, _| Some(Action::Dismiss),
        ),
    ]
}

#[cfg(test)]
mod tests {
    use super::super::{Context, parse_at};
    use super::*;

    const QUIET: Situation = Situation {
        asking: false,
        music: false,
        timer: false,
    };

    #[test]
    fn resolves_by_situation() {
        let asking = Situation {
            asking: true,
            timer: true,
            music: true,
        };
        let timer = Situation {
            timer: true,
            music: true,
            ..QUIET
        };
        let music = Situation {
            music: true,
            ..QUIET
        };
        for (word, s, want) in [
            (Bare::Stop, asking, Action::Dismiss),
            (Bare::Cancel, asking, Action::Dismiss),
            (Bare::Stop, timer, Action::Timer(Timer::Cancel)),
            (Bare::Cancel, timer, Action::Timer(Timer::Cancel)),
            (Bare::Stop, music, Action::Media(Media::Stop)),
            // Cancelling a request must not kill the music.
            (Bare::Cancel, music, Action::Dismiss),
            (Bare::Stop, QUIET, Action::Dismiss),
            (Bare::Pause, timer, Action::Timer(Timer::Pause)),
            (Bare::Pause, music, Action::Media(Media::Pause)),
            (Bare::Resume, QUIET, Action::Media(Media::Play)),
        ] {
            assert_eq!(resolve(word, &s), want, "{word:?} {s:?}");
        }
    }

    #[test]
    fn phrasings() {
        let mut ctx = Context::at(0);
        ctx.situation.timer = true;
        let cancel = Some(Action::Timer(Timer::Cancel));
        for (text, want) in [
            ("Stop!", &cancel),
            ("stop it now!", &cancel),
            ("Cancel.", &cancel),
            ("Alexa, cancel.", &cancel),
            ("Cancel it, please.", &cancel),
            ("oops cancel that", &cancel),
            ("please cancel that last command", &cancel),
            ("abort mission", &cancel),
            ("Pause.", &Some(Action::Timer(Timer::Pause))),
            ("never mind", &Some(Action::Dismiss)),
            ("never mind, don't do that", &Some(Action::Dismiss)),
            (
                "actually forget that i don't need it",
                &Some(Action::Dismiss),
            ),
            ("forget what i told you to do", &Some(Action::Dismiss)),
            ("stop, i changed my mind", &Some(Action::Dismiss)),
            ("Nothing.", &Some(Action::Dismiss)),
            ("Cancel the timer.", &cancel),
        ] {
            assert_eq!(&parse_at(text, &ctx), want, "{text:?}");
        }
        assert_eq!(
            parse_at("Stop!", &Context::at(0)),
            Some(Action::Dismiss),
            "nothing to stop"
        );
    }
}
