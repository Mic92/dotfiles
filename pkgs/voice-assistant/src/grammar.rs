//! Template grammar for intents, in the spirit of Home Assistant's hassil.
//!
//! A template is a sequence of
//! - `word`          literal
//! - `(a|b)`         one of several words
//! - `[a|b]`         optional word
//! - `{slot}`        one or more words accepted by the slot's parser
//!
//! A leading `!` anchors the template: apart from fillers ("alexa",
//! "please") the utterance must consist of the template alone. Unanchored
//! templates may be surrounded by other words; the match ignoring the fewest
//! non-filler words wins, so "stop the music" beats a generic "stop".

use std::collections::HashMap;

use crate::clock::Clock;
use std::cmp::Reverse;
use std::mem;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Duration(u32),
    Clock(Clock),
    /// Days from today.
    Day(u32),
    Number(u32),
    Real(f64),
    /// Index into a skill's own table (apps, ...).
    Index(usize),
    /// Free text the action does not need ("should I {any}").
    Any,
    /// Free text, e.g. a place name.
    Text(String),
    /// Days from today and an index into wttr.in's 3-hourly forecast.
    DayPart(u32, usize),
}

/// Validates the words a slot would consume; `next` is the word after them.
pub type SlotParser = fn(&[&str], Option<&str>) -> Option<Value>;

pub type Slots = HashMap<String, Value>;

const FILLERS: &[&str] = &[
    "alexa", "please", "hey", "ok", "okay", "um", "uh", "so", "now", "just", "could", "can",
    "would", "you", "thanks", COMMA,
];

/// Kept by `normalize` so lists survive ("milk, eggs and bread"); only the
/// `{text}` slot sees it, everything else skips it.
const COMMA: &str = ",";

fn skip_commas(words: &[&str], mut wi: usize) -> usize {
    while words.get(wi) == Some(&COMMA) {
        wi += 1;
    }
    wi
}

/// Alternatives, each one or more words ("do not").
type Alts = Vec<Vec<String>>;

enum Part {
    Word(Alts),
    Optional(Alts),
    /// Name, and whether it may be left out (`[{day}]`).
    Slot(String, bool),
}

/// Split a template into tokens, keeping bracketed groups whole.
fn tokens(src: &str) -> Result<Vec<&str>, String> {
    let mut out = Vec::new();
    let mut rest = src.trim_start();
    while !rest.is_empty() {
        let end = match rest.as_bytes()[0] {
            b'(' => rest.find(')').map(|i| i + 1),
            b'[' => rest.find(']').map(|i| i + 1),
            b'{' => rest.find('}').map(|i| i + 1),
            _ => Some(rest.find(char::is_whitespace).unwrap_or(rest.len())),
        }
        .ok_or_else(|| format!("unclosed group in template {src:?}"))?;
        out.push(&rest[..end]);
        rest = rest[end..].trim_start();
    }
    Ok(out)
}

pub struct Template {
    parts: Vec<Part>,
    anchored: bool,
}

impl Template {
    pub fn parse(src: &str) -> Result<Self, String> {
        let (anchored, body) = match src.strip_prefix('!') {
            Some(rest) => (true, rest),
            None => (false, src),
        };
        // Longest first: "(another|another one)" must not stop after "another".
        let alts = |s: &str| -> Alts {
            let mut a: Alts = s
                .split('|')
                .map(|a| a.split_whitespace().map(str::to_owned).collect())
                .collect();
            a.sort_by_key(|w: &Vec<String>| Reverse(w.len()));
            a
        };
        let parts = tokens(body)?
            .into_iter()
            .map(|tok| {
                if let Some(inner) = tok.strip_prefix('(').and_then(|t| t.strip_suffix(')')) {
                    Ok(Part::Word(alts(inner)))
                } else if let Some(name) = tok.strip_prefix("[{").and_then(|t| t.strip_suffix("}]"))
                {
                    Ok(Part::Slot(name.to_owned(), true))
                } else if let Some(inner) = tok.strip_prefix('[').and_then(|t| t.strip_suffix(']'))
                {
                    Ok(Part::Optional(alts(inner)))
                } else if let Some(name) = tok.strip_prefix('{').and_then(|t| t.strip_suffix('}')) {
                    Ok(Part::Slot(name.to_owned(), false))
                } else if tok.chars().all(|c| c.is_alphanumeric() || c == '\'') {
                    Ok(Part::Word(vec![vec![tok.to_owned()]]))
                } else {
                    Err(format!("bad token {tok:?} in template {src:?}"))
                }
            })
            .collect::<Result<Vec<_>, _>>()?;
        if parts.is_empty() {
            return Err(format!("empty template {src:?}"));
        }
        Ok(Template { parts, anchored })
    }

    /// The plainest sentences this template accepts when its one required
    /// slot takes each of `values(slot)`: shortest alternatives, nothing
    /// optional. Empty when a slot has no fixed set of values.
    pub fn sentences(
        &self,
        values: &impl Fn(&str) -> Option<Vec<Vec<&'static str>>>,
    ) -> Vec<String> {
        let mut prefix: Vec<&str> = Vec::new();
        let mut suffix: Vec<&str> = Vec::new();
        let mut options: Option<Vec<Vec<&'static str>>> = None;
        for part in &self.parts {
            let out = if options.is_some() {
                &mut suffix
            } else {
                &mut prefix
            };
            match part {
                Part::Word(alts) => {
                    let Some(shortest) = alts.iter().min_by_key(|a| a.len()) else {
                        return Vec::new();
                    };
                    out.extend(shortest.iter().map(String::as_str));
                }
                Part::Optional(_) | Part::Slot(_, true) => {}
                Part::Slot(name, false) => {
                    if options.is_some() {
                        return Vec::new();
                    }
                    let Some(v) = values(name) else {
                        return Vec::new();
                    };
                    options = Some(v);
                }
            }
        }
        options
            .unwrap_or_default()
            .iter()
            .map(|v| {
                prefix
                    .iter()
                    .chain(v)
                    .chain(&suffix)
                    .copied()
                    .collect::<Vec<_>>()
                    .join(" ")
            })
            .collect()
    }

    /// Up to `cap` sentences spread over every combination of alternatives,
    /// optional words and slot `values`; training data for classifiers.
    #[cfg(test)]
    pub fn expand(&self, values: &impl Fn(&str) -> Option<Vec<String>>, cap: usize) -> Vec<String> {
        let mut choices: Vec<Vec<String>> = Vec::new();
        for part in &self.parts {
            let join = |a: &Alts| a.iter().map(|w| w.join(" ")).collect::<Vec<_>>();
            choices.push(match part {
                Part::Word(a) => join(a),
                Part::Optional(a) => std::iter::once(String::new()).chain(join(a)).collect(),
                Part::Slot(name, optional) => {
                    let Some(v) = values(name) else {
                        return Vec::new();
                    };
                    let none = optional.then(String::new);
                    none.into_iter().chain(v).collect()
                }
            });
        }
        let total = choices
            .iter()
            .try_fold(1usize, |n, c| n.checked_mul(c.len()))
            .unwrap_or(usize::MAX);
        let n = total.min(cap);
        (0..n)
            .map(|k| {
                // Evenly spaced indices, decoded as mixed-radix numbers.
                let mut i = if n == total { k } else { k * (total / n) };
                let words: Vec<&str> = choices
                    .iter()
                    .map(|c| {
                        let w = &c[i % c.len()];
                        i /= c.len();
                        w.as_str()
                    })
                    .filter(|w| !w.is_empty())
                    .collect();
                words.join(" ")
            })
            .collect()
    }

    /// Best match in `words`: the number of ignored non-filler words and the slots.
    pub fn find(
        &self,
        words: &[&str],
        slots: &impl Fn(&str) -> Option<SlotParser>,
    ) -> Option<(usize, Slots)> {
        let ignored = |ws: &[&str]| ws.iter().filter(|w| !FILLERS.contains(w)).count();
        (0..words.len())
            .filter_map(|start| {
                let mut out = Slots::new();
                let end = self.match_at(0, words, start, slots, &mut out)?;
                let skipped = ignored(&words[..start]) + ignored(&words[end..]);
                (!self.anchored || skipped == 0).then_some((skipped, out))
            })
            .min_by_key(|(skipped, _)| *skipped)
    }

    /// One of `alts` at words[wi..], then the rest of the template.
    fn match_alts(
        &self,
        alts: &Alts,
        pi: usize,
        words: &[&str],
        wi: usize,
        slots: &impl Fn(&str) -> Option<SlotParser>,
        out: &mut Slots,
    ) -> Option<usize> {
        let wi = skip_commas(words, wi);
        let rest = words.get(wi..)?;
        alts.iter().find_map(|alt| {
            let matches = alt.len() <= rest.len() && alt.iter().zip(rest).all(|(a, w)| a == w);
            matches.then(|| self.match_at(pi + 1, words, wi + alt.len(), slots, out))?
        })
    }

    /// Match parts[pi..] at words[wi..]; returns the end position.
    fn match_at(
        &self,
        pi: usize,
        words: &[&str],
        wi: usize,
        slots: &impl Fn(&str) -> Option<SlotParser>,
        out: &mut Slots,
    ) -> Option<usize> {
        let Some(part) = self.parts.get(pi) else {
            return Some(wi);
        };
        match part {
            Part::Word(alts) => self.match_alts(alts, pi, words, wi, slots, out),
            Part::Optional(alts) => self
                .match_alts(alts, pi, words, wi, slots, out)
                .or_else(|| self.match_at(pi + 1, words, wi, slots, out)),
            Part::Slot(name, optional) => {
                let parser = slots(name)?;
                let wi = skip_commas(words, wi);
                // Longest first, so "{duration} timer" still finds "timer".
                let found = (wi + 1..=words.len()).rev().find_map(|end| {
                    let part = &words[wi..end];
                    if part.last() == Some(&COMMA) {
                        return None;
                    }
                    let next = words.get(skip_commas(words, end)).copied();
                    let value = if name == "text" {
                        parser(part, next)?
                    } else {
                        let bare: Vec<&str> =
                            part.iter().copied().filter(|w| *w != COMMA).collect();
                        parser(&bare, next)?
                    };
                    let rest = self.match_at(pi + 1, words, end, slots, out)?;
                    out.insert(name.clone(), value);
                    Some(rest)
                });
                if found.is_none() && *optional {
                    return self.match_at(pi + 1, words, wi, slots, out);
                }
                found
            }
        }
    }
}

/// Lowercase words without punctuation. Hyphens split ("twenty-five"), as do
/// digit/letter boundaries ("10min", "7pm"); "1:30" and "o'clock" stay whole.
pub fn normalize(text: &str) -> Vec<String> {
    // Arithmetic symbols become words before punctuation is dropped.
    let lower = text
        .to_lowercase()
        .replace('+', " plus ")
        .replace(['×', '*'], " times ")
        .replace('÷', " divided by ")
        .replace('=', " ");
    // A comma between words becomes its own token; "1,000" is a number.
    let chars: Vec<char> = lower.chars().collect();
    let lower: String = chars
        .iter()
        .enumerate()
        .map(|(i, &c)| {
            let digit = |j: Option<usize>| {
                j.and_then(|j| chars.get(j))
                    .is_some_and(char::is_ascii_digit)
            };
            if c != ',' {
                c.to_string()
            } else if digit(i.checked_sub(1)) && digit(Some(i + 1)) {
                String::new()
            } else {
                " , ".to_owned()
            }
        })
        .collect();
    let mut words: Vec<String> = Vec::new();
    for raw in lower.split(|c: char| !(c.is_alphanumeric() || matches!(c, '.' | '\'' | ':' | ',')))
    {
        let w = raw.trim_matches(|c| matches!(c, '.' | '\'' | ':'));
        let w = match w {
            "a.m" => "am",
            "p.m" => "pm",
            other => other,
        };
        // Whisper writes "7.30" for 7:30; one decimal ("1.5") stays a number.
        let dotted_time = w.split_once('.').is_some_and(|(h, m)| {
            (1..=2).contains(&h.len())
                && m.len() == 2
                && (h.to_owned() + m).bytes().all(|b| b.is_ascii_digit())
        });
        let w = if dotted_time {
            w.replacen('.', ":", 1)
        } else {
            w.to_owned()
        };
        let w = w.as_str();
        let mut cur = String::new();
        for c in w.chars() {
            if let Some(prev) = cur.chars().last()
                && (prev.is_ascii_digit() && c.is_alphabetic()
                    || prev.is_alphabetic() && c.is_ascii_digit())
            {
                words.push(mem::take(&mut cur));
            }
            cur.push(c);
        }
        if !cur.is_empty() && !(cur == COMMA && words.last().is_none_or(|l| l == COMMA)) {
            words.push(cur);
        }
    }
    if words.last().is_some_and(|l| l == COMMA) {
        words.pop();
    }
    words
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sentences_for_closed_slots() {
        let hosts = |name: &str| (name == "host").then(|| vec![vec!["eve"], vec!["blob", "64"]]);
        let t = |s| {
            Template::parse(s)
                .map(|t| t.sentences(&hosts))
                .unwrap_or_default()
        };
        assert_eq!(
            t("!(can you|please) (reach|ping) [the] {host} [now]"),
            ["please reach eve", "please reach blob 64"]
        );
        assert_eq!(t("ping {host}"), ["ping eve", "ping blob 64"]);
        assert!(t("say {text}").is_empty());
        assert!(t("ping {host} and {host}").is_empty());
        assert!(t("what time is it").is_empty());
    }

    #[test]
    fn normalize_splits() {
        assert_eq!(
            normalize("Set a 10-minute timer."),
            ["set", "a", "10", "minute", "timer"]
        );
        assert_eq!(
            normalize("1h30m, 7pm!"),
            ["1", "h", "30", "m", ",", "7", "pm"]
        );
        assert_eq!(
            normalize("Alexa, milk, , eggs,"),
            ["alexa", ",", "milk", ",", "eggs"]
        );
        assert_eq!(normalize(", 1,000 euros"), ["1000", "euros"]);
        assert_eq!(normalize("At 1:30 p.m."), ["at", "1:30", "pm"]);
        assert_eq!(normalize("Six o'clock"), ["six", "o'clock"]);
        assert_eq!(normalize("1.5 hours..."), ["1.5", "hours"]);
        assert_eq!(normalize("Timer for 7.30."), ["timer", "for", "7:30"]);
        assert_eq!(normalize("at 19.45"), ["at", "19:45"]);
        assert_eq!(normalize("What is 5+3?"), ["what", "is", "5", "plus", "3"]);
        assert_eq!(normalize("7×6"), ["7", "times", "6"]);
    }

    #[test]
    fn phrase_alternatives() {
        let none = |_: &str| -> Option<SlotParser> { None };
        let t = Template::parse("(don't|do not) [go to] sleep").expect("parses");
        assert!(t.find(&["do", "not", "sleep"], &none).is_some());
        assert!(t.find(&["don't", "go", "to", "sleep"], &none).is_some());
        assert!(t.find(&["do", "sleep"], &none).is_none());
    }

    #[test]
    fn optional_slots() {
        let slots = |n: &str| -> Option<SlotParser> {
            (n == "day").then_some(|w: &[&str], _: Option<&str>| {
                (w == ["tomorrow"]).then_some(Value::Day(1))
            })
        };
        let t = Template::parse("!am i busy [{day}]").expect("parses");
        let (_, s) = t
            .find(&["am", "i", "busy", "tomorrow"], &slots)
            .expect("with");
        assert_eq!(s.get("day"), Some(&Value::Day(1)));
        let (_, s) = t.find(&["am", "i", "busy"], &slots).expect("without");
        assert!(s.is_empty());
        assert!(t.find(&["am", "i", "busy", "later"], &slots).is_none());
    }

    #[test]
    fn templates_reject_garbage() {
        assert!(Template::parse("set {x").is_err());
        assert!(Template::parse("set (a|b").is_err());
        assert!(Template::parse("!").is_err());
    }
}
