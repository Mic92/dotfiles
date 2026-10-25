//! Reminders, todo and shopping lists (todoman) and quick notes.

use super::json::{key, text as json_text};
use super::{Action, Context, Intent, Module, Reply, Skill, intent, output, pick, spawn};
use crate::grammar::{Slots, Value};
use crate::store;
use std::env;
use std::path::PathBuf;
use tinyjson::JsonValue;

#[derive(Debug, Clone, PartialEq)]
pub enum Notes {
    Remind {
        what: String,
        seconds: u32,
    },
    /// Index into `LISTS`.
    Add {
        items: Vec<String>,
        list: usize,
    },
    /// Check items off: bought or removed.
    Done {
        items: Vec<String>,
        list: usize,
    },
    Contains {
        item: String,
        list: usize,
    },
    Show(usize),
    Note(String),
}

const TODO: usize = 0;
const SHOPPING: usize = 1;

/// Spoken names and todoman list names.
pub const LISTS: &[(&[&str], &str)] = &[
    (&["todo", "to do", "task", "tasks", "personal"], "Personal"),
    (
        &["shopping", "grocery", "groceries", "shopping and grocery"],
        "Shopping List (Shannan Lekwati)",
    ),
    (
        &["movie", "movies", "film", "films", "tv", "watch"],
        "Movies and TV (Shannan Lekwati)",
    ),
    (
        &["restaurant", "restaurants"],
        "Restaurants To Visit (Shannan Lekwati)",
    ),
    (&["gift", "gifts", "present", "presents"], "Geschenke"),
    (&["places", "travel"], "Places to go (Shannan Lekwati)"),
];

pub fn list_lookup(words: &[&str]) -> Option<usize> {
    let name = words.join(" ");
    LISTS
        .iter()
        .position(|(names, _)| names.contains(&name.as_str()))
}

/// Phrases whose "and" is part of one item.
const COMPOUNDS: &[&str] = &[
    "salt and pepper",
    "mac and cheese",
    "fish and chips",
    "peanut butter and jelly",
    "sweet and sour",
    "salt and vinegar",
    "oil and vinegar",
    "bread and butter",
];

/// Leading words that are not part of the item.
const FILLER: &[&str] = &[
    "some", "a", "an", "the", "more", "few", "any", "also", "of", "new",
];

/// "some milk, eggs and a loaf of bread" -> ["milk", "eggs", "loaf of bread"].
pub fn split_items(text: &str) -> Vec<String> {
    let mut text = text.replace([',', ';'], " and ");
    // Shield compounds from the split.
    for c in COMPOUNDS {
        text = text.replace(c, &c.replace(" and ", "\u{1}"));
    }
    let mut out: Vec<String> = Vec::new();
    for chunk in text.split(" and ").flat_map(|c| c.split(" & ")) {
        let words: Vec<&str> = chunk.split_whitespace().collect();
        let start = words
            .iter()
            .position(|w| !FILLER.contains(w))
            .unwrap_or(words.len());
        let end = (start..words.len())
            .find(|&i| is_instruction(&words[i..]))
            .unwrap_or(words.len());
        let item = words[start..end].join(" ").replace('\u{1}', " and ");
        if !item.is_empty() && !out.iter().any(|o| same_item(o, &item)) {
            out.push(item);
        }
    }
    out
}

/// "add that to my list" or "so forget that" after the item.
fn is_instruction(words: &[&str]) -> bool {
    const VERBS: &[&str] = &["add", "put", "remind", "need", "so", "forget", "please"];
    words.first().is_some_and(|w| VERBS.contains(w))
}

/// "Milk " and "milk" are the same thing; so are "egg" and "eggs".
fn same_item(a: &str, b: &str) -> bool {
    let norm = |s: &str| {
        let s = s.trim().to_lowercase();
        match s.strip_suffix("es").or_else(|| s.strip_suffix('s')) {
            Some(stem) => stem.to_owned(),
            None => s,
        }
    };
    norm(a) == norm(b)
}

fn n(x: Notes) -> Option<Action> {
    Some(Action::Notes(x))
}

fn text(s: &Slots, k: &str) -> Option<String> {
    match s.get(k)? {
        Value::Text(t) => Some(t.clone()),
        _ => None,
    }
}

fn items(s: &Slots) -> Option<Vec<String>> {
    let v = split_items(&text(s, "text")?);
    (!v.is_empty()).then_some(v)
}

/// The spoken list, or `default` when none was named.
fn list(s: &Slots, default: usize) -> Option<usize> {
    match s.get("list") {
        Some(Value::Index(i)) => Some(*i),
        Some(_) => None,
        None => Some(default),
    }
}

pub const MODULE: Module = Module {
    intents,
    slots: &[("list", |w, _| list_lookup(w).map(Value::Index))],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            &[
                "!remind me to {text} in {duration}",
                "!remind me in {duration} to {text}",
            ],
            |s, _| match s.get("duration")? {
                Value::Duration(seconds) => n(Notes::Remind {
                    what: text(s, "text")?,
                    seconds: *seconds,
                }),
                _ => None,
            },
        ),
        intent(
            &[
                "!(add|put|write) {text} (to|on|onto) [the|my|our] {list} list",
                "!(add|put|write) {text} (to|on|onto) [the|my|our] {list}",
                "!(add|put|write) {text} (to|on|onto) [the|my|our] list",
            ],
            |s, _| {
                n(Notes::Add {
                    items: items(s)?,
                    list: list(s, TODO)?,
                })
            },
        ),
        intent(
            &[
                "!(we are|we're|i'm|i am) (out of|running out of|low on|almost out of) {text} (so|and) [will|can|could|would] [you] [please] (add|put|write) (that|it|them|those) (to|on|onto) [the|my|our] [shopping|grocery] list",
                "!(we are|we're|i'm|i am) (out of|running out of|low on|almost out of) {text} (will|can|could|would) you [please] (add|put|write) (that|it|them|those) (to|on|onto) [the|my|our] [shopping|grocery] list",
                "!(we|i) (ran|run) out of {text} (so|and) [will|can|could|would] [you] [please] (add|put|write) (that|it|them|those) (to|on|onto) [the|my|our] [shopping|grocery] list",
                "!(we|i) (ran|run) out of {text} (will|can|could|would) you [please] (add|put|write) (that|it|them|those) (to|on|onto) [the|my|our] [shopping|grocery] list",
                "!(i|we) (need|should|have|must) [to] (buy|get) {text}",
                "!we need {text}",
                "!(we are|we're|i'm|i am) (out of|running out of|low on|almost out of) {text}",
                "!(we|i) (ran|run) out of {text}",
                "!(remember|don't forget|do not forget) to (buy|get|pick up) {text}",
                "!(pick up|get|grab|buy) {text} (from|at) the (store|shop|supermarket)",
                "!(buy|pick up) {text}",
            ],
            |s, _| {
                n(Notes::Add {
                    items: items(s)?,
                    list: SHOPPING,
                })
            },
        ),
        intent(
            &[
                "!(take|cross|scratch|strike) (off|out|away) [the] {text} (from|off) [the|my|our] {list} list",
                "!(take|cross|scratch|strike) (off|out|away) [the] {text} (from|off) [the|my|our] list",
                "!(remove|delete|take|cross|scratch) {text} (from|off) [the|my|our] {list} list",
                "!(remove|delete|take|cross|scratch) {text} (from|off) [the|my|our] {list}",
                "!(remove|delete|take|cross|scratch) {text} (from|off) [the|my|our] list",
                "!(tick|check|cross) off {text}",
            ],
            |s, _| {
                n(Notes::Done {
                    items: items(s)?,
                    list: list(s, SHOPPING)?,
                })
            },
        ),
        intent(
            &[
                "!(i|we) (no longer|don't|do not) need {text} [anymore|any more]",
                "!(i|we) (bought|got|picked up) {text} [already]",
                "!(i|we) already (bought|got) {text}",
            ],
            |s, _| {
                n(Notes::Done {
                    items: items(s)?,
                    list: SHOPPING,
                })
            },
        ),
        intent(
            &[
                "!(is|are) {text} (on|in) [the|my|our] {list} list",
                "!(is|are) {text} (on|in) [the|my|our] {list}",
                "!(did|have) (i|we|you) (add|added|put) {text} [on|to] [the|my|our] {list} [list]",
                "!do (i|we) (need|have to buy|still need) {text}",
            ],
            |s, _| {
                n(Notes::Contains {
                    item: text(s, "text")?,
                    list: list(s, SHOPPING)?,
                })
            },
        ),
        intent(
            &[
                "!what (do|should) (i|we) (buy|get) [from|at] [the] [store|shop|supermarket]",
                "!what (do|should) (i|we) need [to buy|to get] [from|at] [the] [store|shop|supermarket]",
            ],
            |_, _| n(Notes::Show(SHOPPING)),
        ),
        intent(
            &[
                "!(what's|what is) on [the|my|our] {list} list",
                "!(read|show|tell) [me] [the|my|our] {list} list",
                "!what (do i have to do|are my todos|are my tasks)",
            ],
            |s, _| n(Notes::Show(list(s, TODO)?)),
        ),
        intent(
            &[
                "!(take|make) a note [that] {text}",
                "!note [that|down] {text}",
                "!write down [that] {text}",
            ],
            |s, _| n(Notes::Note(text(s, "text")?)),
        ),
    ]
}

impl Skill for Notes {
    /// "and eggs" after adding milk.
    fn refine(&self, words: &[&str]) -> Option<Action> {
        let items = split_items(&words.join(" "));
        if items.is_empty() {
            return None;
        }
        match self {
            Notes::Add { list, .. } => Some(Action::Notes(Notes::Add { items, list: *list })),
            Notes::Done { list, .. } => Some(Action::Notes(Notes::Done { items, list: *list })),
            _ => None,
        }
    }

    fn run(&self, _ctx: &Context) -> Reply {
        let x = self;
        match x {
            Notes::Remind { what, seconds } => remind(what, *seconds),
            Notes::Add { items, list } => add(items, *list),
            Notes::Done { items, list } => done(items, *list),
            Notes::Contains { item, list } => contains(item, *list),
            Notes::Show(list) => show(*list),
            Notes::Note(what) => note(what),
        }
    }
}

fn exe() -> String {
    env::current_exe().map_or_else(|_| "voice-assistant".into(), |p| p.display().to_string())
}

fn remind(what: &str, seconds: u32) -> Reply {
    let when = format!("--on-active={seconds}s");
    let msg = format!("Reminder: {what}");
    // Transient user timers survive the assistant restarting.
    spawn(
        "systemd-run",
        &[
            "--user",
            "--collect",
            "--quiet",
            &when,
            "notify-send",
            "-u",
            "critical",
            "Reminder",
            what,
        ],
    );
    spawn(
        "systemd-run",
        &[
            "--user",
            "--collect",
            "--quiet",
            &when,
            &exe(),
            "--say",
            &msg,
        ],
    );
    let mins = seconds.div_ceil(60);
    let delay = match (seconds, mins) {
        (s, _) if s < 60 => format!("{s} seconds"),
        (_, 1) => "a minute".into(),
        (_, m) => format!("{m} minutes"),
    };
    let speech = pick(
        &[
            "Okay, in {}, I'll nag you.",
            "Got it. {} from now, you'll hear from me.",
            "Fine. I'll remind you in {}. Don't make me repeat myself.",
            "In {}, I'll be in your ear. Like a conscience, but louder.",
        ],
        &delay,
    );
    Reply::live("Reminder", &format!("{what} in {delay}"), speech)
}

fn list_info(list: usize) -> Option<(&'static str, &'static str)> {
    LISTS
        .get(list)
        .map(|(names, name)| (names.first().copied().unwrap_or("todo"), *name))
}

/// "milk", "milk and eggs", "milk, eggs and bread".
fn spoken_list(items: &[String]) -> String {
    match items {
        [] => String::new(),
        [one] => one.clone(),
        [init @ .., last] => format!("{} and {last}", init.join(", ")),
    }
}

fn unknown_list() -> Reply {
    Reply::live(
        "Lists",
        "",
        pick(
            &[
                "Which list? I don't know that one.",
                "That list doesn't exist. Making things up again?",
            ],
            "",
        ),
    )
}

/// `todo --porcelain list` is a JSON array of objects with "id" and "summary".
fn entries(json: &str) -> Option<Vec<(String, String)>> {
    let JsonValue::Array(items) = json.parse().ok()? else {
        return None;
    };
    Some(
        items
            .iter()
            .filter_map(|i| {
                let id = match key(i, "id")? {
                    JsonValue::Number(n) => format!("{n}"),
                    v => json_text(v)?.to_owned(),
                };
                Some((id, json_text(key(i, "summary")?)?.to_owned()))
            })
            .collect(),
    )
}

/// Open items as (todoman id, summary).
fn open_items(list_name: &str) -> Vec<(String, String)> {
    output("todo", &["--porcelain", "list", list_name])
        .and_then(|j| entries(&j))
        .unwrap_or_default()
}

fn add(items: &[String], list: usize) -> Reply {
    let Some((kind, list_name)) = list_info(list) else {
        return unknown_list();
    };
    let existing = open_items(list_name);
    let (dupes, fresh): (Vec<&String>, Vec<&String>) = items
        .iter()
        .partition(|i| existing.iter().any(|(_, e)| same_item(e, i)));
    let added: Vec<String> = fresh
        .into_iter()
        .filter(|i| output("todo", &["new", "--list", list_name, i]).is_some())
        .cloned()
        .collect();
    let dupes: Vec<String> = dupes.into_iter().cloned().collect();
    let (what, dup) = (spoken_list(&added), spoken_list(&dupes));
    let speech = match (added.is_empty(), dupes.is_empty()) {
        (true, true) => "Todoman refused. Maybe the list is gone?".to_owned(),
        (true, false) => pick(
            &[
                "{} is already on the list. Twice won't make it taste better.",
                "Already there: {}. Your memory, darling.",
            ],
            &dup,
        ),
        (false, false) => format!("Added {what}. {dup} was already on there."),
        (false, true) => match kind {
            "shopping" if added.len() >= 4 => pick(
                &[
                    "Added {}. Are you feeding an army?",
                    "Added {}. Shopping is cardio, technically.",
                    "{}. That's a whole trolley, babe.",
                ],
                &what,
            ),
            "shopping" => pick(
                &[
                    "{} is on the shopping list.",
                    "Added {}. Buy me something nice too.",
                    "{}. Got it. Don't eat it all on the way home.",
                ],
                &what,
            ),
            "movie" => pick(
                &["{} added. Popcorn not included.", "Movie night: {}. Noted."],
                &what,
            ),
            _ => pick(
                &[
                    "Added {}. Now go do it.",
                    "Added {}. Procrastination level: list maker.",
                    "{} is on your list. Procrastinate responsibly.",
                ],
                &what,
            ),
        },
    };
    let mut body: Vec<String> = added.iter().map(|i| format!("+ {i}")).collect();
    body.extend(dupes.iter().map(|i| format!("= {i} (already there)")));
    Reply::live(list_name, &body.join("\n"), speech)
}

fn done(items: &[String], list: usize) -> Reply {
    let Some((_, list_name)) = list_info(list) else {
        return unknown_list();
    };
    let existing = open_items(list_name);
    let mut gone = Vec::new();
    let mut missing = Vec::new();
    for item in items {
        match existing.iter().find(|(_, e)| same_item(e, item)) {
            Some((id, name)) if output("todo", &["done", id]).is_some() => gone.push(name.clone()),
            _ => missing.push(item.clone()),
        }
    }
    let speech = match (gone.is_empty(), missing.is_empty()) {
        (true, _) => pick(
            &[
                "{} wasn't on the list anyway.",
                "Can't find {} on there. Did you dream it?",
            ],
            &spoken_list(&missing),
        ),
        (false, true) => pick(
            &[
                "Crossed off {}.",
                "{}, gone. One less thing.",
                "Done with {}. Proud of you.",
                "{} gone. Look at you, adulting.",
            ],
            &spoken_list(&gone),
        ),
        (false, false) => format!(
            "Crossed off {}. {} wasn't on there.",
            spoken_list(&gone),
            spoken_list(&missing)
        ),
    };
    Reply::live(
        list_name,
        &gone
            .iter()
            .map(|i| format!("✓ {i}"))
            .collect::<Vec<_>>()
            .join("\n"),
        speech,
    )
}

fn contains(item: &str, list: usize) -> Reply {
    let Some((_, list_name)) = list_info(list) else {
        return unknown_list();
    };
    let speech = if open_items(list_name)
        .iter()
        .any(|(_, e)| same_item(e, item))
    {
        pick(
            &["Yes, {} is on there.", "{}? Already on the list, sweetie."],
            item,
        )
    } else {
        pick(
            &[
                "No {} on the list. Want it? Just say add it.",
                "Nope, no {}. Living dangerously.",
                "No {}. Panic buying not yet necessary.",
            ],
            item,
        )
    };
    Reply::live(list_name, item, speech)
}

fn show(list: usize) -> Reply {
    let Some((_, list_name)) = list_info(list) else {
        return unknown_list();
    };
    let items: Vec<String> = open_items(list_name).into_iter().map(|(_, s)| s).collect();
    let speech = match items.as_slice() {
        [] => pick(
            &[
                "Nothing on there. Suspiciously productive.",
                "Empty. Go treat yourself.",
                "Nothing. Either very organised or in denial.",
            ],
            "",
        ),
        [one] => format!("Just one thing: {one}."),
        [_, _] | [_, _, _] => format!("{}.", spoken_list(&items)),
        [a, b, c, ..] => format!(
            "{} things. Starting with {a}, {b} and {c}. The rest is on screen.",
            items.len()
        ),
    };
    Reply::live(list_name, &items.join("\n"), speech)
}

fn note(what: &str) -> Reply {
    let Some(home) = env::var_os("HOME") else {
        return Reply::live(
            "Note",
            "",
            pick(
                &[
                    "I don't know where you live. Literally.",
                    "No home directory. Are you a ghost?",
                ],
                "",
            ),
        );
    };
    let path = PathBuf::from(home).join("notes/inbox.md");
    let stamp = output("date", &["+%Y-%m-%d %H:%M"]).unwrap_or_default();
    let res = store::append(&path, &format!("- {stamp} {what}"));
    match res {
        Ok(()) => Reply::live(
            "Note",
            what,
            pick(
                &[
                    "Noted.",
                    "Written down. I won't tell anyone.",
                    "Got it. It's in your inbox file.",
                    "Noted. I'll pretend I didn't read it.",
                ],
                "",
            ),
        ),
        Err(e) => Reply::live(
            "Note",
            &e.to_string(),
            pick(
                &[
                    "I couldn't write that down. My pen broke.",
                    "Writing failed. The note was probably too spicy.",
                ],
                "",
            ),
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    fn add(items: &[&str], list: usize) -> Notes {
        Notes::Add {
            items: items.iter().map(|&i| i.to_owned()).collect(),
            list,
        }
    }

    fn done(items: &[&str]) -> Notes {
        Notes::Done {
            items: items.iter().map(|&i| i.to_owned()).collect(),
            list: SHOPPING,
        }
    }

    #[test]
    fn phrasings() {
        let p = |t| parse_at(t, &Context::at(0));
        for (text, want) in [
            (
                "Remind me to call mum in 10 minutes.",
                Notes::Remind {
                    what: "call mum".into(),
                    seconds: 600,
                },
            ),
            (
                "Remind me in an hour to check the oven",
                Notes::Remind {
                    what: "check the oven".into(),
                    seconds: 3600,
                },
            ),
            ("Add milk to the shopping list.", add(&["milk"], SHOPPING)),
            ("Put Dune on my movie list", add(&["dune"], 2)),
            ("Add fix the CI to my todo list", add(&["fix the ci"], TODO)),
            (
                "Add some milk, eggs and a loaf of bread to the shopping list",
                add(&["milk", "eggs", "loaf of bread"], SHOPPING),
            ),
            (
                "Add salt and pepper to shopping",
                add(&["salt and pepper"], SHOPPING),
            ),
            (
                "Put bananas on the grocery list.",
                add(&["bananas"], SHOPPING),
            ),
            ("We're out of eggs.", add(&["eggs"], SHOPPING)),
            ("We're running out of coffee", add(&["coffee"], SHOPPING)),
            (
                "We ran out of toilet paper.",
                add(&["toilet paper"], SHOPPING),
            ),
            ("I'm low on milk", add(&["milk"], SHOPPING)),
            (
                "I'm out of kleenex so will you put that on my shopping list",
                add(&["kleenex"], SHOPPING),
            ),
            (
                "I'm low on cheerios, can you add that to the shopping list",
                add(&["cheerios"], SHOPPING),
            ),
            (
                "I ran out of apples so add them to my list",
                add(&["apples"], SHOPPING),
            ),
            ("Take off fries from the shopping list", done(&["fries"])),
            (
                "Please take away the fries from the shopping list",
                done(&["fries"]),
            ),
            ("We no longer need fries", done(&["fries"])),
            (
                "I'm out of apples, add that to my list",
                add(&["apples"], SHOPPING),
            ),
            (
                "I'm out of bananas; add to shopping list",
                add(&["bananas"], SHOPPING),
            ),
            (
                "I am out of trash bags and need them to be added to my shopping list",
                add(&["trash bags"], SHOPPING),
            ),
            ("I no longer need milk, so forget that", done(&["milk"])),
            (
                "Cross out bananas from my shopping list",
                done(&["bananas"]),
            ),
            ("I need to buy batteries", add(&["batteries"], SHOPPING)),
            (
                "Don't forget to buy cat food.",
                add(&["cat food"], SHOPPING),
            ),
            (
                "Pick up 2 litres of milk from the store",
                add(&["2 litres of milk"], SHOPPING),
            ),
            (
                "We need more butter and cheese",
                add(&["butter", "cheese"], SHOPPING),
            ),
            ("Remove milk from the shopping list", done(&["milk"])),
            ("Take eggs off the shopping list", done(&["eggs"])),
            ("I bought milk and eggs.", done(&["milk", "eggs"])),
            (
                "Is milk on my shopping list?",
                Notes::Contains {
                    item: "milk".into(),
                    list: SHOPPING,
                },
            ),
            (
                "Do we need eggs?",
                Notes::Contains {
                    item: "eggs".into(),
                    list: SHOPPING,
                },
            ),
            ("What's on my shopping list?", Notes::Show(SHOPPING)),
            ("What do I need to buy?", Notes::Show(SHOPPING)),
            ("What do we need from the store?", Notes::Show(SHOPPING)),
            ("What do I have to do?", Notes::Show(TODO)),
            (
                "Take a note that the wifi password is on the fridge",
                Notes::Note("the wifi password is on the fridge".into()),
            ),
        ] {
            assert_eq!(p(text), Some(Action::Notes(want)), "{text:?}");
        }
    }

    #[test]
    fn todo_json() {
        let j = r#"[{"id":3,"summary":"Milk"},{"id":"7","summary":" Eggs "},{"id":9}]"#;
        assert_eq!(
            entries(j),
            Some(vec![
                ("3".into(), "Milk".into()),
                ("7".into(), "Eggs".into())
            ])
        );
        assert_eq!(entries("garbage"), None);
    }

    #[test]
    fn item_splitting() {
        assert_eq!(
            split_items("milk, eggs, and bread"),
            ["milk", "eggs", "bread"]
        );
        assert_eq!(
            split_items("salt and pepper and mac and cheese"),
            ["salt and pepper", "mac and cheese"]
        );
        assert_eq!(split_items("some milk and more milk"), ["milk"]);
        assert_eq!(split_items("an egg and eggs"), ["egg"]);
        assert!(split_items("a").is_empty());
        assert!(same_item("Eggs", "egg"));
        assert!(same_item(" Milk", "milk"));
        assert!(!same_item("milk", "silk"));
        assert_eq!(
            spoken_list(&["a".into(), "b".into(), "c".into()]),
            "a, b and c"
        );
    }
}
