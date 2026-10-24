//! Desktop toggles and actions via noctalia and niri.

use super::{Action, Context, Intent, Module, Reply, Skill, intent, noctalia, pick, spawn};
use crate::grammar::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Desktop {
    Brighter,
    Darker,
    Brightness(u32),
    NightLight(bool),
    Wifi(bool),
    Bluetooth(bool),
    DoNotDisturb(bool),
    Screenshot,
    ScreenshotRegion,
    Wallpaper,
    /// Keep the screen on.
    Caffeine(bool),
    PowerProfile(&'static str),
    Launcher,
    ClearNotifications,
    CloseWindow,
    Fullscreen,
}

fn d(x: Desktop) -> Option<Action> {
    Some(Action::Desktop(x))
}

pub const MODULE: Module = Module {
    intents,
    slots: &[],
    vocab: &[],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            None,
            &[
                "(make|turn) [the] (screen|display|brightness) (brighter|up)",
                "(turn|crank) up the brightness",
                "brightness up",
                "!brighter",
            ],
            |_, _| d(Desktop::Brighter),
        ),
        intent(
            None,
            &[
                "(make|turn) [the] (screen|display|brightness) (darker|dimmer|down)",
                "turn down the brightness",
                "dim the (screen|display)",
                "brightness down",
                "!(darker|dimmer)",
            ],
            |_, _| d(Desktop::Darker),
        ),
        intent(
            None,
            &[
                "[set|change] [the] brightness (to|at) {number} [percent]",
                "brightness {number} [percent]",
            ],
            |s, _| match s.get("number")? {
                Value::Real(n) if (0.0..=100.0).contains(n) => {
                    // Range-checked, so the cast is exact.
                    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                    d(Desktop::Brightness(n.round() as u32))
                }
                _ => None,
            },
        ),
        intent(
            None,
            &[
                "(turn|switch) on [the] night (light|mode)",
                "(enable|start) [the] night (light|mode)",
                "night (light|mode) on",
                "!night (light|mode)",
            ],
            |_, _| d(Desktop::NightLight(true)),
        ),
        intent(
            None,
            &[
                "(turn|switch) off [the] night (light|mode)",
                "(disable|stop) [the] night (light|mode)",
                "night (light|mode) off",
            ],
            |_, _| d(Desktop::NightLight(false)),
        ),
        intent(
            None,
            &[
                "(turn|switch) on [the] (wifi|wi|wireless) [fi]",
                "enable [the] (wifi|wi|wireless) [fi]",
                "(wifi|wi) [fi] on",
            ],
            |_, _| d(Desktop::Wifi(true)),
        ),
        intent(
            None,
            &[
                "(turn|switch) off [the] (wifi|wi|wireless) [fi]",
                "disable [the] (wifi|wi|wireless) [fi]",
                "(wifi|wi) [fi] off",
            ],
            |_, _| d(Desktop::Wifi(false)),
        ),
        intent(
            None,
            &[
                "(turn|switch) on [the] bluetooth",
                "enable [the] bluetooth",
                "bluetooth on",
            ],
            |_, _| d(Desktop::Bluetooth(true)),
        ),
        intent(
            None,
            &[
                "(turn|switch) off [the] bluetooth",
                "disable [the] bluetooth",
                "bluetooth off",
            ],
            |_, _| d(Desktop::Bluetooth(false)),
        ),
        intent(
            None,
            &[
                "(turn|switch) on do not disturb",
                "(enable|activate) do not disturb",
                "!do not disturb [mode] [on]",
                "!dnd",
                "!focus mode",
            ],
            |_, _| d(Desktop::DoNotDisturb(true)),
        ),
        intent(
            None,
            &[
                "(turn|switch) off do not disturb",
                "(disable|deactivate) do not disturb",
                "do not disturb off",
            ],
            |_, _| d(Desktop::DoNotDisturb(false)),
        ),
        intent(
            None,
            &[
                "(take|make|grab) a screenshot",
                "!screenshot",
                "screenshot [of] [the] [whole] screen",
            ],
            |_, _| d(Desktop::Screenshot),
        ),
        intent(
            None,
            &[
                "screenshot [of] [a|the] (region|area|part|selection)",
                "(take|make) a (region|partial) screenshot",
            ],
            |_, _| d(Desktop::ScreenshotRegion),
        ),
        intent(
            None,
            &[
                "(change|switch|next|new|random) [the|my] wallpaper",
                "(give|show) me a (new|different) wallpaper",
            ],
            |_, _| d(Desktop::Wallpaper),
        ),
        intent(
            None,
            &[
                "keep the (screen|display|computer) (on|awake)",
                "(don't|do not) (sleep|lock|dim)",
                "(turn|switch) on caffeine",
                "!caffeine",
            ],
            |_, _| d(Desktop::Caffeine(true)),
        ),
        intent(
            None,
            &[
                "(turn|switch) off caffeine",
                "let the (screen|display|computer) sleep",
            ],
            |_, _| d(Desktop::Caffeine(false)),
        ),
        intent(
            None,
            &[
                "(power|battery|energy) (saving|saver) [mode]",
                "save (power|battery|energy)",
            ],
            |_, _| d(Desktop::PowerProfile("power-saver")),
        ),
        intent(
            None,
            &["performance mode", "(full|max|maximum) (power|performance)"],
            |_, _| d(Desktop::PowerProfile("performance")),
        ),
        intent(None, &["balanced [power] mode"], |_, _| {
            d(Desktop::PowerProfile("balanced"))
        }),
        intent(
            None,
            &[
                "(open|show) [the] [app|application] launcher",
                "show [me] [the|my] apps",
            ],
            |_, _| d(Desktop::Launcher),
        ),
        intent(
            None,
            &["(clear|dismiss|delete) [all] [the|my] notifications"],
            |_, _| d(Desktop::ClearNotifications),
        ),
        intent(
            None,
            &["close [this|the] (window|app|application)"],
            |_, _| d(Desktop::CloseWindow),
        ),
        intent(
            None,
            &[
                "(make|go|toggle) [it|this] fullscreen",
                "make the window fullscreen",
                "!fullscreen",
                "maximize [this|the] window",
            ],
            |_, _| d(Desktop::Fullscreen),
        ),
    ]
}

const DONE: &[&str] = &[
    "Done.",
    "As you wish.",
    "Your wish is my command.",
    "Consider it done, gorgeous.",
    "Done. You owe me one.",
    "Sorted. What would you do without me?",
    "There. Was that so hard? For me, no.",
];
const BRIGHTER: &[&str] = &[
    "Let there be light!",
    "Brighter. Like your future.",
    "Ow, my eyes. Done.",
    "Brighter. Now I can see your bad posture.",
    "Lights up. Hide the evidence.",
];
const DARKER: &[&str] = &[
    "Dimming. Very romantic.",
    "Darker. Setting the mood?",
    "Lights down, vibe up.",
    "Dimmer. Hiding something?",
    "Darker. Candles not included.",
];
const NIGHT: &[&str] = &[
    "Night light on. Cozy.",
    "Warm colours activated. Bedtime soon, okay?",
    "Warm light. Your eyes say thank you, your bedtime says go.",
];
const DND: &[&str] = &[
    "Do not disturb. Not even me? Okay, not even me.",
    "Shields up. Nobody gets through.",
    "Do not disturb. Except me. I'm the exception.",
    "Ghost mode. Everyone else can wait.",
];
const SCREENSHOT: &[&str] = &[
    "Cheese!",
    "Snap! Hope there's nothing embarrassing on there.",
    "Got it. Evidence secured.",
    "Screenshot taken. Blackmail folder updated.",
    "Click. Say cheese next time.",
];
const WALLPAPER: &[&str] = &[
    "New wallpaper. Ooh, fancy.",
    "Redecorating. Do you like it?",
    "Fresh wallpaper. Fresh start. Same old you.",
];
const CAFFEINE: &[&str] = &[
    "Caffeine on. I'm wide awake now. Too awake.",
    "Screen stays on. No naps for you.",
    "The screen will stay on. Unlike your attention span.",
];
const CLOSE: &[&str] = &[
    "Closed. Bye bye window.",
    "Gone. I never liked that one anyway.",
    "Closed. It had it coming.",
    "Window gone. Your secrets with it.",
];
const OFF: &[&str] = &["Turned off.", "Off it goes.", "Done. Off."];

impl Skill for Desktop {
    fn run(&self, _ctx: &Context) -> Reply {
        let x = self;
        let lines = match x {
            Desktop::Brighter => {
                noctalia(&["brightness-up"]);
                BRIGHTER
            }
            Desktop::Darker => {
                noctalia(&["brightness-down"]);
                DARKER
            }
            Desktop::Brightness(n) => {
                noctalia(&["brightness-set", &n.to_string()]);
                DONE
            }
            Desktop::NightLight(on) => toggle("nightlight", *on, NIGHT),
            Desktop::Wifi(on) => toggle("wifi", *on, DONE),
            Desktop::Bluetooth(on) => toggle("bluetooth", *on, DONE),
            Desktop::DoNotDisturb(on) => {
                noctalia(&["notification-dnd-set", if *on { "on" } else { "off" }]);
                if *on { DND } else { OFF }
            }
            Desktop::Screenshot => {
                noctalia(&["screenshot-fullscreen"]);
                SCREENSHOT
            }
            Desktop::ScreenshotRegion => {
                noctalia(&["screenshot-region"]);
                SCREENSHOT
            }
            Desktop::Wallpaper => {
                noctalia(&["wallpaper-random"]);
                WALLPAPER
            }
            Desktop::Caffeine(on) => toggle("caffeine", *on, CAFFEINE),
            Desktop::PowerProfile(p) => {
                noctalia(&["power-set", p]);
                DONE
            }
            Desktop::Launcher => {
                noctalia(&["panel-toggle", "launcher"]);
                DONE
            }
            Desktop::ClearNotifications => {
                noctalia(&["notification-clear-history"]);
                DONE
            }
            Desktop::CloseWindow => {
                spawn("niri", &["msg", "action", "close-window"]);
                CLOSE
            }
            Desktop::Fullscreen => {
                spawn("niri", &["msg", "action", "fullscreen-window"]);
                DONE
            }
        };
        Reply::new("Desktop", "", pick(lines, ""))
    }

    fn repeatable(&self) -> bool {
        matches!(
            self,
            Desktop::Brighter | Desktop::Darker | Desktop::Wallpaper
        )
    }
}

/// `<name>-enable` / `<name>-disable`; turning off gets the plain replies.
fn toggle(name: &str, on: bool, on_lines: &'static [&'static str]) -> &'static [&'static str] {
    noctalia(&[&format!("{name}-{}", if on { "enable" } else { "disable" })]);
    if on { on_lines } else { OFF }
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    #[test]
    fn phrasings() {
        let p = |t| parse_at(t, &Context::at(0));
        for (text, want) in [
            ("Make the screen brighter.", Desktop::Brighter),
            ("Brighter!", Desktop::Brighter),
            ("Dim the screen", Desktop::Darker),
            ("Set the brightness to 40 percent", Desktop::Brightness(40)),
            ("Turn on the night light.", Desktop::NightLight(true)),
            ("Night mode off", Desktop::NightLight(false)),
            ("Turn off the wifi.", Desktop::Wifi(false)),
            ("Turn on the Wi-Fi", Desktop::Wifi(true)),
            ("Turn on bluetooth", Desktop::Bluetooth(true)),
            ("Do not disturb.", Desktop::DoNotDisturb(true)),
            ("Turn off do not disturb", Desktop::DoNotDisturb(false)),
            ("Take a screenshot.", Desktop::Screenshot),
            ("Screenshot of a region", Desktop::ScreenshotRegion),
            ("Change the wallpaper", Desktop::Wallpaper),
            ("Keep the screen on.", Desktop::Caffeine(true)),
            ("Power saving mode", Desktop::PowerProfile("power-saver")),
            ("Open the launcher", Desktop::Launcher),
            ("Clear all notifications.", Desktop::ClearNotifications),
            ("Close this window", Desktop::CloseWindow),
            ("Fullscreen.", Desktop::Fullscreen),
        ] {
            assert_eq!(p(text), Some(Action::Desktop(want)), "{text:?}");
        }
    }
}
