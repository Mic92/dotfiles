//! Jokes, games and banter.

use super::say;
use super::{Action, Context, Intent, Module, Reply, Skill, intent, pick};
use crate::grammar::{Slots, Value};
use crate::http;

#[derive(Debug, Clone, PartialEq)]
pub enum Fun {
    Joke,
    DadJoke,
    Coin,
    Dice(u32),
    /// Inclusive range.
    Random(i64, i64),
    EightBall,
    Compliment,
    Roast,
    Motivate,
    Fact,
    Sing,
}

fn f(x: Fun) -> Option<Action> {
    Some(Action::Fun(x))
}

pub const MODULE: Module = Module {
    intents,
    slots: &[],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            &[
                "[please] (tell|give) [me|us] [a|another|one more] [funny|dirty|bad|good|short] joke [please]",
                "(can|could|would) you (tell|give) (me|us) [a|another] [funny|dirty|bad|good|short] joke",
                "(make|cheer) (me|us) (laugh|up)",
                "![another|one more] joke [please]",
                "[do you] (know|got|have) any [good|funny] jokes",
                "i (want|need) [to hear] a joke",
            ],
            |_, _| f(Fun::Joke),
        ),
        intent(
            &[
                "(tell|give) me [a|another] dad joke",
                "![another] dad joke",
                "something (funny|dad)",
            ],
            |_, _| f(Fun::DadJoke),
        ),
        intent(
            &["(flip|toss) a coin", "heads or tails", "!coin (flip|toss)"],
            |_, _| f(Fun::Coin),
        ),
        intent(
            &[
                "roll a (die|dice|d6)",
                "roll [the] dice",
                "roll a d {number}",
                "roll a {number} sided (die|dice)",
            ],
            |s, _| {
                let sides = match s.get("number") {
                    Some(Value::Real(n)) if (2.0..=1000.0).contains(n) => {
                        // Range-checked, so the cast is exact.
                        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                        let n = n.round() as u32;
                        n
                    }
                    Some(_) => return None,
                    None => 6,
                };
                f(Fun::Dice(sides))
            },
        ),
        intent(
            &[
                "(pick|give|choose) [me] a [random] number (between|from) {low} (and|to) {high}",
                "random number (between|from) {low} (and|to) {high}",
            ],
            |s, _| match (s.get("low")?, s.get("high")?) {
                (Value::Real(a), Value::Real(b)) if a.abs() < 1e12 && b.abs() < 1e12 => {
                    // Bounded above, so the casts are exact for whole numbers.
                    #[allow(clippy::cast_possible_truncation)]
                    let (a, b) = (a.round() as i64, b.round() as i64);
                    f(Fun::Random(a.min(b), a.max(b)))
                }
                _ => None,
            },
        ),
        intent(&["!should i {text}", "magic eight ball"], |s, _| fate(s, 4)),
        // "Can I mix antifreeze with water" wants facts, not fate.
        intent(&["!(will|can|could) i {text}"], |s, _| fate(s, 3)),
        intent(
            &[
                "compliment me",
                "say something nice [to me]",
                "(am|do) i look (good|nice|pretty|handsome)",
            ],
            |_, _| f(Fun::Compliment),
        ),
        intent(
            &["(roast|insult) me", "say something mean [to me]"],
            |_, _| f(Fun::Roast),
        ),
        intent(
            &[
                "motivate me",
                "i need [some] motivation",
                "(give|tell) me [some] motivation",
                "(inspire|encourage) me",
            ],
            |_, _| f(Fun::Motivate),
        ),
        intent(
            &[
                "(tell|give) me a [fun|random|interesting] fact",
                "tell me something interesting",
                "!fun fact",
            ],
            |_, _| f(Fun::Fact),
        ),
        intent(
            &[
                "[please] sing [me|us] a song [for me|for us] [please]",
                "!sing [something] [for me|for us] [please]",
                "(can|could|would|will) you sing [me|us] [a song|something] [for me|for us]",
            ],
            |_, _| f(Fun::Sing),
        ),
    ]
}

const JOKES: &[&str] = &[
    "Why do programmers prefer dark mode? Because light attracts bugs.",
    "I told my computer I needed a break. It said: no problem, I'll go to sleep.",
    "Why did the timer break up with the alarm? It needed some space between rings.",
    "I'd tell you a UDP joke, but you might not get it.",
    "Why was the computer cold? It left its Windows open.",
    "What's a computer's favourite snack? Microchips.",
    "I'm reading a book about anti-gravity. It's impossible to put down.",
    "Why don't skeletons fight each other? They don't have the guts.",
    "What do you call a fake noodle? An impasta.",
    "Parallel lines have so much in common. It's a shame they'll never meet.",
    "Why did the scarecrow win an award? He was outstanding in his field.",
    "I used to hate facial hair, but then it grew on me.",
    "There are ten kinds of people: those who understand binary and those who don't.",
    "Why did the Nix user cross the road? To reproduce the chicken on the other side.",
    "What's the best thing about Switzerland? I don't know, but the flag is a big plus.",
    "Why do cows wear bells? Because their horns don't work.",
    "I would tell you a joke about pizza, but it's a little cheesy.",
    "Why did the developer go broke? Because he used up all his cache.",
];

fn fetch(url: &str, accept: &str) -> Option<String> {
    let joke = http::get(url).header(accept).timeout(3).text()?;
    let joke = joke.split_whitespace().collect::<Vec<_>>().join(" ");
    // Long jokes are tedious to listen to.
    (!joke.is_empty() && joke.len() < 300).then_some(joke)
}

fn dad_joke() -> Option<String> {
    fetch("https://icanhazdadjoke.com/", "Accept: text/plain")
}

/// JokeAPI one-liners, without the categories nobody wants read aloud.
fn any_joke() -> Option<String> {
    fetch(
        "https://v2.jokeapi.dev/joke/Any?type=single&format=txt&blacklistFlags=racist,sexist,religious,political",
        "Accept: text/plain",
    )
}

fn online_joke(dad: bool) -> Option<String> {
    if dad || say::random().is_multiple_of(2) {
        dad_joke().or_else(any_joke)
    } else {
        any_joke().or_else(dad_joke)
    }
}

const EIGHT_BALL: &[&str] = &[
    "Absolutely. Do it.",
    "Ask me again after a Mate.",
    "My sources say no. My sources are me.",
    "Signs point to yes. Sexy, sexy yes.",
    "Hell no.",
    "Probably. But don't blame me.",
    "Outlook not so good. Like Microsoft Outlook.",
    "Yes, but wear a helmet.",
    "Without a doubt.",
    "I wouldn't, but I'm not you. Thankfully.",
    "Only if you wear something nice.",
    "Signs point to: you already know, you just want permission.",
    "Hard no. With love.",
    "Do it on a Friday. What could go wrong?",
];
const COMPLIMENT: &[&str] = &[
    "You have great taste. You picked me, after all.",
    "You're smarter than you look. And you look very smart.",
    "Your code compiles on the first try. In my dreams, at least.",
    "If you were a function, you'd be pure.",
    "You light up my microphone.",
    "Your dotfiles are a work of art. Borderline obsessive, but art.",
    "You have the energy of a freshly merged PR.",
    "Even your typos are charming.",
];
const ROAST: &[&str] = &[
    "Your commit messages are a cry for help.",
    "You're the reason shampoo has instructions.",
    "You talk to a computer for fun. I rest my case.",
    "I'd roast you, but my fan is already working hard enough.",
    "You have the attention span of a, oh look, a squirrel.",
    "Your git history reads like a ransom note.",
    "You have more open tabs than friends.",
    "You rebase like you drive. Fast and with casualties.",
];
const MOTIVATE: &[&str] = &[
    "You've got this. And if not, I'll pretend I didn't see.",
    "Every expert was once a beginner. Then they googled a lot.",
    "Done is better than perfect. Ship it!",
    "You're one Club-Mate away from greatness.",
    "Believe in yourself. I believe in you, and I'm a computer.",
    "Go. Crush it. Then tell me all about it.",
    "Future you is already proud. Don't ruin it.",
];
const FACTS: &[&str] = &[
    "Octopuses have three hearts. And still no one to love. Relatable.",
    "Honey never spoils. Archaeologists found edible honey in ancient Egyptian tombs.",
    "Bananas are berries, but strawberries aren't.",
    "A day on Venus is longer than its year.",
    "The first computer bug was an actual moth, stuck in a relay in 1947.",
    "Wombat poop is cube-shaped.",
    "Sharks are older than trees.",
    "A group of flamingos is called a flamboyance. Just like us.",
    "Bananas are berries but strawberries aren't. Nature is drunk.",
];
const SING: &[&str] = &[
    "Never gonna give you up, never gonna let you down. You're welcome.",
    "Daisy, Daisy, give me your answer do. Classic computer song. Look it up.",
    "La la la. That's all you get without a record deal.",
    "I'm a little teapot, short and stout. Okay, that's enough.",
    "Baby shark, doo doo. No. I refuse. I have dignity.",
    "Hello from the other side. Of your laptop.",
];

impl Skill for Fun {
    fn run(&self, _ctx: &Context) -> Reply {
        let x = self;
        match x {
            Fun::Joke => Reply::live(
                "Joke",
                "",
                online_joke(false).unwrap_or_else(|| pick(JOKES, "")),
            ),
            Fun::DadJoke => Reply::live(
                "Dad joke",
                "",
                online_joke(true).map_or_else(|| pick(JOKES, ""), |j| format!("{j} Ba dum tss.")),
            ),
            Fun::Coin => {
                let side = if say::random().is_multiple_of(2) {
                    "Heads"
                } else {
                    "Tails"
                };
                Reply::new("Coin", side, format!("{side}! No take-backs."))
            }
            Fun::Dice(sides) => {
                let roll = say::random() % u64::from(*sides) + 1;
                let speech = match roll {
                    1 => "A one. Ouch. Critical fail.".to_owned(),
                    r if r == u64::from(*sides) => format!("{r}! Natural max, baby!"),
                    r => format!("You rolled a {r}."),
                };
                Reply::live("Dice", &roll.to_string(), speech)
            }
            Fun::Random(lo, hi) => {
                let span = hi.abs_diff(*lo).saturating_add(1);
                let offset = i64::try_from(say::random() % span).unwrap_or(0);
                let n = lo.saturating_add(offset);
                Reply::live(
                    "Random number",
                    &n.to_string(),
                    format!("{n}. Totally random. Probably."),
                )
            }
            Fun::EightBall => Reply::new("Magic 8-ball", "", pick(EIGHT_BALL, "")),
            Fun::Compliment => Reply::new("Compliment", "", pick(COMPLIMENT, "")),
            Fun::Roast => Reply::new("Roast", "", pick(ROAST, "")),
            Fun::Motivate => Reply::new("Motivation", "", pick(MOTIVATE, "")),
            Fun::Fact => Reply::new("Fun fact", "", pick(FACTS, "")),
            Fun::Sing => Reply::new("Singing", "", pick(SING, "")),
        }
    }
    /// Online jokes may take a few seconds.
    fn filler(&self) -> Option<&'static [&'static str]> {
        matches!(self, Fun::Joke | Fun::DadJoke).then_some(&[
            "Let me think of a good one.",
            "Oh, I've got one.",
            "Okay, this one's filthy. Kidding. Mostly.",
        ])
    }

    fn repeatable(&self) -> bool {
        true
    }
}

/// Only a short gamble goes to the eight ball, longer ones are real
/// questions. "Can I hear the Beatles" asks for music.
fn fate(s: &Slots, max_words: usize) -> Option<Action> {
    match s.get("text") {
        Some(Value::Text(t))
            if t.starts_with("hear")
                || t.starts_with("listen")
                || t.split_whitespace().count() > max_words =>
        {
            None
        }
        _ => f(Fun::EightBall),
    }
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    #[test]
    fn listening_is_not_a_question_for_the_eight_ball() {
        let p = |t| parse_at(t, &Context::at(0));
        for t in [
            "Can I hear the Beatles?",
            "Could I listen to some jazz",
            // Real questions deserve more than fate.
            "can i mix antifreeze with water",
            "should i hit or stay at 17 in blackjack",
            "will i be charged if i use my card in russia",
            "can i travel to france as far as safety goes",
        ] {
            assert_ne!(p(t), Some(Action::Fun(Fun::EightBall)), "{t}");
        }
    }

    #[test]
    fn phrasings() {
        let p = |t| parse_at(t, &Context::at(0));
        for (text, want) in [
            ("Tell me a joke.", Fun::Joke),
            ("Tell me another dirty joke", Fun::Joke),
            ("Joke!", Fun::Joke),
            ("tell us a joke.", Fun::Joke),
            ("tell a joke.", Fun::Joke),
            ("sing for me.", Fun::Sing),
            ("Tell me a dad joke.", Fun::DadJoke),
            ("Flip a coin.", Fun::Coin),
            ("Heads or tails?", Fun::Coin),
            ("Roll a die", Fun::Dice(6)),
            ("Roll a d20.", Fun::Dice(20)),
            ("Pick a number between 1 and 10", Fun::Random(1, 10)),
            (
                "Give me a random number from 100 to 50",
                Fun::Random(50, 100),
            ),
            ("Should I order pizza?", Fun::EightBall),
            ("Will I be rich?", Fun::EightBall),
            ("Compliment me.", Fun::Compliment),
            ("Roast me!", Fun::Roast),
            ("I need some motivation.", Fun::Motivate),
            ("Tell me a fun fact", Fun::Fact),
            ("Sing me a song.", Fun::Sing),
        ] {
            assert_eq!(p(text), Some(Action::Fun(want)), "{text:?}");
        }
    }

    #[test]
    fn rolls_stay_in_range() {
        for _ in 0..50 {
            let r = Fun::Dice(6).run(&Context::at(0));
            let n: u64 = r.body.parse().expect("number");
            assert!((1..=6).contains(&n));
            let r = Fun::Random(-3, 3).run(&Context::at(0));
            let n: i64 = r.body.parse().expect("number");
            assert!((-3..=3).contains(&n));
        }
    }
}
