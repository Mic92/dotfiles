//! Headlines from RSS/Atom feeds.

use super::{Action, Context, Intent, Module, Reply, Skill, intent, pick};
use crate::http;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Feed {
    World,
    Tech,
}

impl Feed {
    fn url(self) -> &'static str {
        match self {
            Feed::World => "https://feeds.bbci.co.uk/news/rss.xml",
            Feed::Tech => "https://hnrss.org/frontpage?points=200",
        }
    }

    fn name(self) -> &'static str {
        match self {
            Feed::World => "BBC News",
            Feed::Tech => "Hacker News",
        }
    }
}

fn n(f: Feed) -> Option<Action> {
    Some(Action::News(f))
}

pub const MODULE: Module = Module {
    intents,
    slots: &[],
    vocab: &[],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            Some("What's the news?"),
            &[
                "!(what's|what is) (the|in the) news [today]",
                "!(what's|what is) happening in the world",
                "!(read|tell|give) me the (news|headlines)",
                "![the|today's] headlines [please]",
                "!any news",
                "!(news|headlines) (update|briefing)",
            ],
            |_, _| n(Feed::World),
        ),
        intent(
            None,
            &[
                "!(what's|what is) (on|new on) hacker news",
                "![read|tell|give] [me] [the] (tech|nerd|computer) (news|headlines)",
                "!hacker news [headlines|please]",
                "!(what's|what is) new in tech",
            ],
            |_, _| n(Feed::Tech),
        ),
    ]
}

fn unescape(s: &str) -> String {
    let s = s.trim();
    let s = s
        .strip_prefix("<![CDATA[")
        .and_then(|s| s.strip_suffix("]]>"))
        .unwrap_or(s);
    s.replace("&amp;", "&")
        .replace("&quot;", "\"")
        .replace("&#39;", "'")
        .replace("&apos;", "'")
        .replace("&lt;", "<")
        .replace("&gt;", ">")
        .trim()
        .to_owned()
}

/// Titles of `<item>` (RSS) or `<entry>` (Atom) elements, skipping the channel title.
fn headlines(xml: &str) -> Vec<String> {
    let mut out = Vec::new();
    for (open, close) in [("<item", "</item>"), ("<entry", "</entry>")] {
        let mut rest = xml;
        while let Some(i) = rest.find(open) {
            let after = &rest[i..];
            let Some(end) = after.find(close) else { break };
            let item = &after[..end];
            if let Some(t) = item.find("<title").and_then(|t| {
                let body = &item[t..];
                let start = body.find('>')? + 1;
                let stop = body.find("</title>")?;
                (start <= stop).then(|| unescape(&body[start..stop]))
            }) && !t.is_empty()
                && !t.starts_with("Livestream")
                && !t.starts_with("Watch:")
            {
                out.push(t);
            }
            rest = &after[end..];
        }
    }
    out
}

const INTRO: &[&str] = &[
    "Here's what's going on. Brace yourself.",
    "The news, in brief, before it gets worse.",
    "Headlines. Read with one eye closed.",
    "The news. Sit down for this one.",
];

impl Skill for Feed {
    fn run(&self, _ctx: &Context) -> Reply {
        let f = *self;
        let items = http::get(f.url())
            .text()
            .map(|x| headlines(&x))
            .unwrap_or_default();
        if items.is_empty() {
            return Reply::live(
                "News",
                "",
                pick(
                    &[
                        "No news. Either the world is fine or the internet is down.",
                        "The news won't load. Blissful ignorance it is.",
                    ],
                    "",
                ),
            );
        }
        let top: Vec<&str> = items.iter().take(3).map(String::as_str).collect();
        let speech = format!("{} {}", pick(INTRO, ""), top.join(". Next: "));
        let body = items
            .iter()
            .take(8)
            .map(|t| format!("• {t}"))
            .collect::<Vec<_>>()
            .join("\n");
        Reply::live(f.name(), &body, speech)
    }

    fn filler(&self) -> Option<&'static [&'static str]> {
        Some(&[
            "Fetching the headlines.",
            "One sec, reading the papers.",
            "Let me see who's ruining the world today.",
        ])
    }

    fn repeatable(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    #[test]
    fn phrasings() {
        let p = |t| parse_at(t, &Context::at(0));
        for (text, want) in [
            ("What's the news?", Feed::World),
            ("Read me the headlines", Feed::World),
            ("Headlines please.", Feed::World),
            ("What's on Hacker News?", Feed::Tech),
            ("Tech news", Feed::Tech),
        ] {
            assert_eq!(p(text), Some(Action::News(want)), "{text:?}");
        }
    }

    #[test]
    fn parses_feeds() {
        let rss = "<rss><channel><title>BBC News</title><item><title><![CDATA[Storm hits &amp; floods]]></title></item>\
                   <item><title>Watch: wind</title></item><item><title>Second</title></item></channel></rss>";
        assert_eq!(headlines(rss), ["Storm hits & floods", "Second"]);
        let atom = r#"<feed><title>feed</title><entry><title type="html">Rust 2.0 &quot;soon&quot;</title></entry></feed>"#;
        assert_eq!(headlines(atom), ["Rust 2.0 \"soon\""]);
        assert!(headlines("garbage").is_empty());
    }
}
