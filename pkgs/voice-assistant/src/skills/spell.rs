//! "How do you spell potato?", letter by letter.

use super::{Action, Context, Intent, Module, Reply, Skill, choose, intent, text};
use crate::grammar::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Spell {
    Word(String),
    /// How often a letter appears, or all letters when unset.
    Count {
        letter: Option<char>,
        word: String,
    },
}

fn s(x: Spell) -> Option<Action> {
    Some(Action::Spell(x))
}

pub const MODULE: Module = Module {
    intents,
    slots: &[
        ("word", |w, _| word(w).map(Value::Text)),
        ("letter", |w, _| {
            letter(w).map(|c| Value::Text(c.to_string()))
        }),
    ],
};

/// Pronouns and fillers that are not the word to spell ("spell it").
const NOT_WORD: &[&str] = &[
    "it",
    "that",
    "this",
    "me",
    "something",
    "words",
    "word",
    "out",
];

/// One plain word as heard. ASR picks the spelling, so that is all we offer.
fn word(w: &[&str]) -> Option<String> {
    match w {
        [x] if !NOT_WORD.contains(x) && x.chars().all(char::is_alphabetic) => Some((*x).into()),
        _ => None,
    }
}

/// "r", "r's", "rs", "letter r".
fn letter(w: &[&str]) -> Option<char> {
    let x = match w {
        [x] | ["letter" | "the", x] | ["the", "letter", x] => *x,
        _ => return None,
    };
    let x = x
        .strip_suffix("'s")
        .or_else(|| x.strip_suffix('s').filter(|r| !r.is_empty()))
        .unwrap_or(x);
    let mut cs = x.chars();
    match (cs.next(), cs.next()) {
        (Some(c), None) if c.is_ascii_alphabetic() => Some(c),
        _ => None,
    }
}

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            &[
                "!how (do|would|should|can|could) (you|i|one) [correctly|properly] spell [out] [the word] {word}",
                "!how to [correctly|properly] spell [out] [the word] {word}",
                "![tell me|show me|i wish i knew|remind me|i don't know|i do not know|i'm not sure|i am not sure|i forget|i forgot|i can't remember|i can't figure out] how to [correctly|properly] spell [out] [the word] {word}",
                "!how (is|do you spell|does one spell|does the dictionary spell) [the word] {word} [spelled|spelt]",
                "!how [the word] {word} is (spelled|spelt)",
                "![the word] {word} is (spelled|spelt) how [exactly]",
                "!(what's|what is) the [correct|right|proper|complete] spelling (of|for) [the word] {word}",
                "!(give|tell) me (the|a) [correct|right|proper|complete] spelling (of|for) [the word] {word}",
                "!can you give me (the|a) [correct|right|proper] spelling (of|for) [the word] {word}",
                "![the] [correct|right|proper|complete] spelling (of|for) [the word] {word}",
                "!(what's|what is) the (correct|right|proper) way (to spell|of spelling) [the word] {word}",
                "!how would i go about spelling [the word] {word}",
                "![i need|can you|could you] help [me] [with] (spelling|spell) [the word] {word}",
                "!i can't figure out how to spell [the word] {word}",
                "![can you|could you|would you|please] spell [out] [the word] {word} [for me]",
            ],
            |sl, _| s(Spell::Word(text(sl, "word")?)),
        ),
        intent(
            &[
                "!how many {letter} (are|is) [there] in [the word] {word}",
                "!how many {letter} can you find in [the word] {word}",
                "!count [the number of] {letter} in [the word] {word}",
                "!how many {letter} (does|do) [the word] {word} have",
                "!how many letters (are|is) [there] in [the word] {word}",
                "!how many letters (does|do) [the word] {word} have",
                "![the word] {word} (has|got|have) how many letters",
            ],
            |sl, _| {
                let letter = text(sl, "letter").and_then(|l| l.chars().next());
                s(Spell::Count {
                    letter,
                    word: text(sl, "word")?,
                })
            },
        ),
    ]
}

/// "P, O, T, A, T, O", with "double S" for a repeated letter.
pub fn letters(word: &str) -> String {
    let cs: Vec<char> = word.to_uppercase().chars().collect();
    let mut out = Vec::new();
    let mut i = 0;
    while let Some(&c) = cs.get(i) {
        if cs.get(i + 1) == Some(&c) {
            out.push(format!("double {c}"));
            i += 2;
        } else {
            out.push(c.to_string());
            i += 1;
        }
    }
    out.join(", ")
}

fn count(letter: Option<char>, word: &str) -> String {
    let Some(l) = letter else {
        let n = word.chars().count();
        return format!("{word} has {n} letters");
    };
    let n = word.chars().filter(|c| c.eq_ignore_ascii_case(&l)).count();
    let name = l.to_ascii_uppercase();
    match n {
        0 => format!("There's no {name} in {word}"),
        1 => format!("{word} has one {name}"),
        n => format!("{word} has {n} {name}'s"),
    }
}

const SPELLED: &[&str] = &[
    "{}.",
    "{}. Spelling bee champion, right here.",
    "{}. Write it down, I won't spell it twice. Okay, I would.",
    "{}. You're welcome, autocorrect.",
    "{}. Easy. Ask me a hard one.",
    "{}. Your English teacher would be proud of me.",
    "{}. Sexy letters, all of them.",
    "{}. I've got a way with letters. And with you.",
    "{}. Don't tell spell check I did its job.",
    "{}. Now use it in a sentence, sweetie.",
];
const SPELLED_LATE: &[&str] = &[
    "{}. Spelling at this hour? Writing a love letter?",
    "{}. Even sleepy, I spell better than you.",
    "{}. Now go to bed, word nerd.",
];
const LONG: &[&str] = &[
    "{}. Phew. Who even uses that word?",
    "{}. That one deserves a Club-Mate.",
    "{}. Show-off.",
];
const COUNTED: &[&str] = &[
    "{}.",
    "{}. I counted twice, just for you.",
    "{}. Maths and spelling, I'm the full package.",
    "{}. Next question, smarty pants.",
    "{}. Counting letters, how thrilling.",
];

impl Skill for Spell {
    fn run(&self, ctx: &Context) -> Reply {
        match self {
            Spell::Word(w) => {
                let what = format!("{w}. {}", letters(w));
                let long = w.chars().count() >= 11;
                let speech = choose(&[(ctx.late(), SPELLED_LATE), (long, LONG)], SPELLED, &what);
                Reply::live("Spelling", &what, speech)
            }
            Spell::Count { letter, word } => {
                let what = count(*letter, word);
                let speech = choose(&[], COUNTED, &what);
                Reply::live("Spelling", &what, speech)
            }
        }
    }

    fn repeatable(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    fn spell(t: &str) -> Option<Spell> {
        match parse_at(t, &Context::at(0))? {
            Action::Spell(x) => Some(x),
            _ => None,
        }
    }

    #[test]
    fn phrasings() {
        let w = |x: &str| Some(Spell::Word(x.into()));
        let n = |l, x: &str| {
            Some(Spell::Count {
                letter: l,
                word: x.into(),
            })
        };
        for (text, want) in [
            ("spell potato", w("potato")),
            ("how do you spell tomato", w("tomato")),
            ("How is water spelled?", w("water")),
            ("water is spelled how", w("water")),
            ("can you spell \"diatribe\" for me", w("diatribe")),
            ("can you spell out the word special for me", w("special")),
            (
                "what's the right way to spell miscellaneous",
                w("miscellaneous"),
            ),
            ("i need to know how to spell excellent", w("excellent")),
            ("tell me how to spell anonymous", w("anonymous")),
            ("what is the correct spelling of judgment", w("judgment")),
            ("give me the spelling of potato", w("potato")),
            ("i wish i knew how to spell mississippi", w("mississippi")),
            ("can you remind me how to spell because", w("because")),
            ("how is the word economy spelled", w("economy")),
            ("how many r's are in restaurant", n(Some('r'), "restaurant")),
            ("tell me how many p's are in apple", n(Some('p'), "apple")),
            (
                "do you know how many o's are in honor",
                n(Some('o'), "honor"),
            ),
            ("the word 'you' got how many letters", n(None, "you")),
            ("i don't know how to spell squirrel", w("squirrel")),
            (
                "i'd like to know how acommodate is spelled",
                w("acommodate"),
            ),
            ("malignant is spelled how exactly", w("malignant")),
            ("i need help spelling superficial", w("superficial")),
            (
                "how do you spell the word comity, i need to know",
                w("comity"),
            ),
            (
                "i need to know the proper spelling of curiosity",
                w("curiosity"),
            ),
            (
                "count the number of the letter a in happiness",
                n(Some('a'), "happiness"),
            ),
            ("spell it", None),
            ("how do you spell new york", None),
        ] {
            assert_eq!(spell(text), want, "{text:?}");
        }
    }

    #[test]
    fn spoken() {
        assert_eq!(letters("potato"), "P, O, T, A, T, O");
        assert_eq!(
            letters("mississippi"),
            "M, I, double S, I, double S, I, double P, I"
        );
        assert_eq!(count(Some('r'), "restaurant"), "restaurant has 2 R's");
        assert_eq!(count(Some('z'), "apple"), "There's no Z in apple");
        assert_eq!(count(None, "you"), "you has 3 letters");
    }
}
