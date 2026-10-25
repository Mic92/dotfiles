//! Web questions answered by Kagi's Quick Answer (`kagi-search -j`).

use pulldown_cmark::{Event, Options, Parser, Tag, TagEnd};
use unicode_segmentation::UnicodeSegmentation;

use super::json::{key, text};
use super::{Action, Context, Intent, Module, Reply, Skill, intent, output, pick};
use crate::grammar::{Slots, Value};
use tinyjson::JsonValue;

/// A web question.
#[derive(Debug, Clone, PartialEq)]
pub struct Query(pub String);

fn ask(s: &Slots, prefix: &str) -> Option<Action> {
    match s.get("text")? {
        Value::Text(t) => Some(Action::Kagi(Query(format!("{prefix}{t}")))),
        _ => None,
    }
}

fn ask_whole(s: &Slots) -> Option<Action> {
    match s.get("question")? {
        Value::Text(t) => Some(Action::Kagi(Query(t.clone()))),
        _ => None,
    }
}

const QUESTION_WORDS: &[&str] = &[
    "who", "who's", "whom", "whose", "what", "what's", "when", "where", "where's", "why", "how",
    "how's", "which",
];

/// A whole question: a question word and at least two more words, so
/// "what?" or "how are you" style fragments stay with chat.
fn question(words: &[&str]) -> Option<Value> {
    let [first, rest @ ..] = words else {
        return None;
    };
    (QUESTION_WORDS.contains(first) && rest.len() >= 2).then(|| Value::Text(words.join(" ")))
}

pub const MODULE: Module = Module {
    intents,
    slots: &[("question", |w, _| question(w))],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            &[
                "!(search|google|kagi|look up) [the web|the internet|online] [for] {text}",
                "!(ask|use) kagi [about] {text}",
                "!(can you|could you|please) (search|google|look up) [for] {text}",
            ],
            |s, _| ask(s, ""),
        ),
        intent(&["!(find out|tell me) {question}"], |s, _| ask_whole(s)),
        // Open questions, asked whole, when nothing else understood them.
        intent(&["!{question}"], |s, _| ask_whole(s)).fallback(),
    ]
}

const MAX_FOOTNOTES: usize = 50;
/// List items read out after a lead-in like "it has two parts:".
const MAX_ITEMS: usize = 5;

/// A paragraph or list item as plain sentences.
struct Block {
    item: bool,
    sentences: Vec<String>,
}

/// Kagi's markdown without headings and footnote references.
fn blocks(md: &str) -> Vec<Block> {
    // Kagi cites [^1].. without defining them, and undefined footnotes
    // parse as plain text.
    let defined: String = (1..=MAX_FOOTNOTES)
        .map(|i| format!("\n\n[^{i}]: -"))
        .collect();
    let md = format!("{md}{defined}");
    let mut out = Vec::new();
    let mut current = String::new();
    let mut item = false;
    let mut skip = 0usize;
    let mut flush = |text: &mut String, item: bool| {
        let sentences: Vec<String> = text
            .split_sentence_bounds()
            .map(|s| s.split_whitespace().collect::<Vec<_>>().join(" "))
            .filter(|s| !s.is_empty())
            .collect();
        text.clear();
        if !sentences.is_empty() {
            out.push(Block { item, sentences });
        }
    };
    for event in Parser::new_ext(&md, Options::ENABLE_FOOTNOTES) {
        match event {
            Event::Start(Tag::Heading { .. } | Tag::FootnoteDefinition(_)) => skip += 1,
            Event::End(TagEnd::Heading(_) | TagEnd::FootnoteDefinition) => {
                skip = skip.saturating_sub(1);
            }
            _ if skip > 0 => {}
            Event::Text(t) | Event::Code(t) => current.push_str(&t),
            // "metres [^2]." must not become "metres ."
            Event::FootnoteReference(_) => current.truncate(current.trim_end().len()),
            Event::SoftBreak | Event::HardBreak => current.push(' '),
            Event::Start(Tag::Item) => {
                flush(&mut current, item);
                item = true;
            }
            Event::End(TagEnd::Item) => {
                flush(&mut current, true);
                item = false;
            }
            Event::End(TagEnd::Paragraph) => flush(&mut current, item),
            _ => {}
        }
    }
    flush(&mut current, item);
    out
}

/// The first `max` sentences; a lead-in ending in a colon is followed by
/// the first sentence of each list item after it.
fn spoken(md: &str, max: usize) -> String {
    let mut said: Vec<&str> = Vec::new();
    let all = blocks(md);
    let mut rest = all.iter().peekable();
    while let Some(b) = rest.next() {
        for s in &b.sentences {
            if said.len() >= max {
                return said.join(" ");
            }
            said.push(s);
        }
        if said.last().is_some_and(|s| s.ends_with(':')) {
            let mut items = 0;
            while let Some(i) = rest.next_if(|n| n.item) {
                if items < MAX_ITEMS {
                    said.extend(i.sentences.first().map(String::as_str));
                    items += 1;
                }
            }
            return said.join(" ");
        }
    }
    said.join(" ")
}

fn parse(raw: &str) -> Option<(String, Option<String>)> {
    let v: JsonValue = raw.parse().ok()?;
    let qa = key(&v, "quick_answer")?;
    let md = key(qa, "markdown")
        .and_then(text)
        .filter(|t| !t.trim().is_empty())?;
    let url = key(qa, "references")
        .and_then(|r| match r {
            JsonValue::Array(a) => a.first(),
            _ => None,
        })
        .and_then(|r| key(r, "url"))
        .and_then(text)
        .map(str::to_owned);
    Some((md.to_owned(), url))
}

const THINKING_FAIL: &[&str] = &[
    "The internet had nothing on that. Rare, but it happens.",
    "Kagi shrugged. I'm shrugging too, in spirit.",
    "Even the internet gave up on that one. Impressive.",
];

impl Skill for Query {
    fn run(&self, _ctx: &Context) -> Reply {
        let q = self.0.as_str();
        let raw = output("kagi-search", &["-j", q]);
        let Some((md, url)) = raw.as_deref().and_then(parse) else {
            return Reply::live("Kagi", q, pick(THINKING_FAIL, ""));
        };
        let speech = spoken(&md, 3);
        let body = match url {
            Some(u) => format!("{}\n{u}", spoken(&md, 6)),
            None => spoken(&md, 6),
        };
        Reply::live(&format!("Kagi: {q}"), &body, speech)
    }

    fn filler(&self) -> Option<&'static [&'static str]> {
        Some(&[
            "Let me ask the internet.",
            "Looking it up.",
            "Hang on, consulting the hive mind.",
            "Googling. Don't tell Kagi I said that.",
        ])
    }
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    #[test]
    fn spoken_continues_past_lead_ins() {
        let md = "The phrase \"**420 blaze it**\" is slang [^2][^3]. Its meaning has two parts:\n\n- **420**: A code term for cannabis [^3]. It also means 4:20 p.m. on April 20th.\n- **Blaze it**: Slang for smoking [^5].\n\n## Usage and Context\nIt is used ironically, e.g. in memes.\n\n[^2]: https://example.com";
        assert_eq!(
            spoken(md, 3),
            "The phrase \"420 blaze it\" is slang. Its meaning has two parts: 420: A code term for cannabis. Blaze it: Slang for smoking."
        );
        assert_eq!(
            spoken("It is used ironically, e.g. in memes. Really.", 1),
            "It is used ironically, e.g. in memes."
        );
        assert_eq!(spoken("", 2), "");
    }

    #[test]
    fn the_question_word_stays() {
        let p = |t| parse_at(t, &Context::at(0));
        for (t, q) in [
            (
                "can you tell me how to clean a fish tank",
                "how to clean a fish tank",
            ),
            (
                "tell me who sells dixie paper plates",
                "who sells dixie paper plates",
            ),
            ("find out why the sky is blue", "why the sky is blue"),
        ] {
            assert_eq!(p(t), Some(Action::Kagi(Query(q.into()))), "{t:?}");
        }
    }

    #[test]
    fn questions_only_as_fallback() {
        let p = |t| parse_at(t, &Context::at(0));
        // A sloppy match of a real skill still beats the catch-all.
        assert!(!matches!(
            p("What's the time, please?"),
            Some(Action::Kagi(_))
        ));
        assert!(!matches!(
            p("How much time is left on the timer?"),
            Some(Action::Kagi(_))
        ));
        assert_eq!(question(&["what", "now"]), None);
        // Mangled commands start with "did" or "is" too often.
        assert_eq!(question(&["did", "you", "just", "eat"]), None);
    }

    #[test]
    fn phrasings() {
        let p = |t| parse_at(t, &Context::at(0));
        for (text, want) in [
            (
                "Search for the best pizza in Munich",
                "the best pizza in munich",
            ),
            ("Look up nix flakes", "nix flakes"),
            ("Ask Kagi about black holes", "black holes"),
            ("Who was Ada Lovelace?", "who was ada lovelace"),
            (
                "How tall is the Eiffel tower?",
                "how tall is the eiffel tower",
            ),
            ("What is a quasar?", "what is a quasar"),
            (
                "How many people live in Germany?",
                "how many people live in germany",
            ),
        ] {
            assert!(
                matches!(p(text), Some(Action::Kagi(Query(q))) if q.ends_with(want)),
                "{text:?} -> {:?}",
                p(text)
            );
        }
        assert!(!matches!(p("What time is it?"), Some(Action::Kagi(_))));
        assert!(!matches!(
            p("What is the weather like?"),
            Some(Action::Kagi(_))
        ));
    }

    #[test]
    fn answer() {
        let raw = r#"{"results": [], "quick_answer": {"markdown": "The **Eiffel Tower** stands at **330 metres** [^2][^3]. It was built in 1889 [^1]. Third sentence.\n\n- **Detail**: more", "references": [{"title": "B", "url": "https://britannica.com/x"}]}}"#;
        let (md, url) = parse(raw).expect("parses");
        assert_eq!(url.as_deref(), Some("https://britannica.com/x"));
        assert_eq!(
            spoken(&md, 2),
            "The Eiffel Tower stands at 330 metres. It was built in 1889."
        );
        assert!(parse(r#"{"quick_answer": {"markdown": ""}}"#).is_none());
    }
}
