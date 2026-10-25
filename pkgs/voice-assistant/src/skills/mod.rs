//! Skills: each module owns its templates, slot parsers, actions and
//! how they run. To add one, write `skills/<name>.rs` with an action type
//! implementing `Skill` and a `pub const MODULE`, then add an `Action`
//! variant and list the module in `MODULES`.

pub mod apps;
pub mod calc;
pub mod chat;
pub mod desktop;
mod exec;
pub mod followup;
pub mod fun;
mod fuzzy;
pub mod info;
mod json;
pub mod kagi;
pub mod media;
pub mod meetings;
pub mod news;
pub mod notes;
pub mod ops;
mod say;
pub mod system;
pub mod timer;
pub mod volume;

use std::sync::LazyLock;

use crate::grammar::{SlotParser, Slots, Template, Value, normalize};
use crate::{clock, duration};

#[derive(Debug, Clone, PartialEq)]
pub enum Action {
    Timer(timer::Timer),
    Media(media::Media),
    Desktop(desktop::Desktop),
    App(apps::Launch),
    Fun(fun::Fun),
    Chat(chat::Chat),
    Ops(ops::Ops),
    Notes(notes::Notes),
    Meeting(meetings::Meeting),
    Kagi(kagi::Query),
    News(news::Feed),
    /// Say the last reply again; the caller remembers it.
    Repeat,
    System(system::System),
    Volume(volume::Volume),
    Info(info::Info),
    Calc(calc::Calc),
    /// Spoken replies on or off ("shut up" / "you can talk again").
    Voice(bool),
    /// The assistant sleeps (ignores everything but "wake up") or listens.
    Listen(bool),
    /// Asleep for this many seconds, then listening again.
    Nap(u32),
    /// "Did you have a nap?"; offers to take one.
    NapQuestion,
    /// "shut down" alone: ask whether the assistant or the computer is meant.
    Which(system::Power),
    /// Just say something, e.g. after a follow-up was declined.
    Say(String),
    /// "never mind": end the conversation without doing anything.
    Dismiss,
}

/// What every skill's action type can do.
pub use exec::{noctalia, output, spawn, succeeds};
use say::{ASLEEP, AWAKE, DISMISS, DISMISS_LATE, SHUT_UP, TALK_AGAIN};
pub use say::{choose, not_understood, pick};

pub trait Skill {
    fn run(&self, ctx: &Context) -> Reply;

    /// Said right away when `run` waits on the network.
    fn filler(&self) -> Option<&'static [&'static str]> {
        None
    }

    /// Whether "again" / "another one" may simply run it once more.
    fn repeatable(&self) -> bool {
        false
    }

    /// "and tomorrow?": this action with a slot replaced by `words`.
    fn refine(&self, _words: &[&str]) -> Option<Action> {
        None
    }
}

impl Action {
    /// The skill behind an action; `None` for the assistant's own ones,
    /// which need the caller's state.
    pub fn skill(&self) -> Option<&dyn Skill> {
        Some(match self {
            Action::Timer(x) => x,
            Action::Media(x) => x,
            Action::Desktop(x) => x,
            Action::App(x) => x,
            Action::Fun(x) => x,
            Action::Chat(x) => x,
            Action::Ops(x) => x,
            Action::Notes(x) => x,
            Action::Meeting(x) => x,
            Action::Kagi(x) => x,
            Action::News(x) => x,
            Action::System(x) => x,
            Action::Volume(x) => x,
            Action::Info(x) => x,
            Action::Calc(x) => x,
            Action::Repeat
            | Action::Voice(_)
            | Action::Listen(_)
            | Action::Nap(_)
            | Action::NapQuestion
            | Action::Which(_)
            | Action::Say(_)
            | Action::Dismiss => return None,
        })
    }
}

/// A skill module's registration.
pub struct Module {
    pub intents: fn() -> Vec<Intent>,
    /// `{name}` slots only this module's templates use.
    pub slots: &'static [(&'static str, SlotParser)],
}

/// Order matters: on equally good matches the earlier module wins, so the
/// catch-all web questions come last.
const MODULES: &[Module] = &[
    timer::MODULE,
    system::MODULE,
    volume::MODULE,
    media::MODULE,
    desktop::MODULE,
    apps::MODULE,
    info::MODULE,
    calc::MODULE,
    fun::MODULE,
    chat::MODULE,
    ops::MODULE,
    notes::MODULE,
    meetings::MODULE,
    news::MODULE,
    kagi::MODULE,
];

/// Interprets the answer to a question we asked, without a wake word.
pub type FollowUp = Box<dyn FnOnce(&str, &Context) -> Option<Action> + Send>;

/// What to tell the user after running an action.
pub struct Reply {
    pub title: String,
    pub body: String,
    pub speech: String,
    pub follow_up: Option<FollowUp>,
    /// Fixed lines are worth caching as audio; live data is not.
    pub cache: bool,
}

impl Reply {
    pub fn new(title: &str, body: &str, speech: String) -> Self {
        Reply {
            title: title.into(),
            body: body.into(),
            speech,
            follow_up: None,
            cache: true,
        }
    }

    /// A reply containing live data (weather, clipboard, ...).
    pub fn live(title: &str, body: &str, speech: String) -> Self {
        Reply {
            cache: false,
            ..Reply::new(title, body, speech)
        }
    }
}

/// State actions and replies may depend on.
pub struct Context {
    /// Seconds since local midnight.
    pub now: u32,
    /// 0 = Sunday.
    pub weekday: u32,
    /// The last action, for "and tomorrow?" or "another one".
    pub previous: Option<Action>,
}

impl Context {
    pub fn local() -> Self {
        Context {
            now: clock::local_now(),
            weekday: clock::local_weekday(),
            previous: None,
        }
    }

    /// A Wednesday at `now`, for tests.
    #[cfg(test)]
    pub const fn at(now: u32) -> Self {
        Context {
            now,
            weekday: 3,
            previous: None,
        }
    }

    pub fn hour(&self) -> u32 {
        self.now / 3600
    }

    /// Up when sensible people sleep.
    pub fn late(&self) -> bool {
        self.hour() >= 23 || self.hour() < 5
    }

    pub fn early(&self) -> bool {
        (5..8).contains(&self.hour())
    }
}

pub struct Intent {
    templates: Vec<Template>,
    #[cfg(test)]
    declared: usize,
    build: fn(&Slots, &Context) -> Option<Action>,
    /// Only tried when no other intent matches at all.
    fallback: bool,
}

impl Intent {
    /// Make this a catch-all that never beats a real match, however sloppy.
    pub fn fallback(mut self) -> Self {
        self.fallback = true;
        self
    }
}

pub fn intent(templates: &[&str], build: fn(&Slots, &Context) -> Option<Action>) -> Intent {
    Intent {
        templates: templates
            .iter()
            // The table is static; `all_templates_parse` catches typos.
            .filter_map(|t| {
                Template::parse(t)
                    .inspect_err(|e| eprintln!("voice-assistant: {e}"))
                    .ok()
            })
            .collect(),
        #[cfg(test)]
        declared: templates.len(),
        build,
        fallback: false,
    }
}

fn core_intents() -> Vec<Intent> {
    vec![
        intent(
            &[
                "!(repeat|say) (that|it) [again]",
                "!what did you say",
                "!say again",
                "!pardon",
            ],
            |_, _| Some(Action::Repeat),
        ),
        intent(
            &["!never mind", "!nevermind", "!forget it", "!nothing"],
            |_, _| Some(Action::Dismiss),
        ),
        intent(
            &[
                "!shut up",
                "!(be|keep) quiet",
                "!(stop|quit) (talking|speaking|chatting)",
                "!(hush|shush|shh|silence)",
                "!zip it",
            ],
            |_, _| Some(Action::Voice(false)),
        ),
        intent(
            &[
                "!(shut|power|turn|switch) yourself (down|off)",
                "!(stop|quit) listening",
                "!take a break",
                "!go away",
                "!(pause|deactivate|disable) yourself",
            ],
            |_, _| Some(Action::Listen(false)),
        ),
        intent(
            &[
                "!(take a break|take a nap|nap|sleep|go to sleep|be quiet|shut up|stop listening|pause|pause yourself) (for|during) {duration}",
                "!(leave me alone|don't listen|do not listen|don't disturb me|do not disturb me) (for|during) {duration}",
            ],
            |s, _| match s.get("duration")? {
                Value::Duration(secs) if *secs <= 12 * 3600 => Some(Action::Nap(*secs)),
                _ => None,
            },
        ),
        intent(
            &[
                "!(did|have) you (had|have) a (nap|sleep|rest|break|siesta) [today]",
                "!are you tired",
            ],
            |_, _| Some(Action::NapQuestion),
        ),
        intent(
            &[
                "!wake up",
                "!(start|resume) listening",
                "!come back",
                "!i need you",
            ],
            |_, _| Some(Action::Listen(true)),
        ),
        intent(
            &[
                "![you] can (talk|speak) again",
                "!(talk|speak) to me [again]",
                "!(talk|speak) again",
                "!unmute yourself",
            ],
            |_, _| Some(Action::Voice(true)),
        ),
    ]
}

static INTENTS: LazyLock<Vec<Intent>> = LazyLock::new(|| {
    let mut all = followup::intents();
    for m in MODULES {
        all.extend((m.intents)());
    }
    all.extend(core_intents());
    all
});

fn slot_parser(name: &str) -> Option<SlotParser> {
    match name {
        "duration" => Some(|w, next| duration::slot(w, next).map(Value::Duration)),
        // Any time of day; only after words like "at" or "until".
        "clock" => Some(|w, _| clock::parse(w).map(Value::Clock)),
        // A time of day that cannot be a duration ("3 pm", not "3").
        "time" => Some(|w, _| clock::parse(w).filter(|c| c.marked).map(Value::Clock)),
        "percent" => Some(|w, _| {
            let n = match w {
                [n] => duration::number(n)?,
                [tens, ones] => {
                    duration::number(tens)? + duration::number(ones).filter(|o| *o < 10)?
                }
                _ => return None,
            };
            (n <= 150).then_some(Value::Number(n))
        }),
        "number" | "low" | "high" => Some(|w, next| {
            // Stop before a following number word so "1 and 10" splits.
            if next.is_some_and(|n| duration::number(n).is_some()) {
                return None;
            }
            duration::number_words(w).map(Value::Real)
        }),
        "text" => Some(|w, _| (!w.is_empty()).then(|| Value::Text(w.join(" ").replace(" ,", ",")))),
        "any" => Some(|_, _| Some(Value::Any)),
        "when" => Some(|w, _| info::day_part(w).map(|(d, p)| Value::DayPart(d, p))),
        "day" => Some(|w, _| match w {
            ["today"] => Some(Value::Day(0)),
            ["tomorrow"] => Some(Value::Day(1)),
            ["day", "after", "tomorrow"] | ["the", "day", "after", "tomorrow"] => {
                Some(Value::Day(2))
            }
            _ => None,
        }),
        _ => MODULES
            .iter()
            .flat_map(|m| m.slots)
            .find(|(n, _)| *n == name)
            .map(|(_, p)| *p),
    }
}

/// Parse `words` as slot `name`, e.g. for `Skill::refine`.
pub fn slot(name: &str, words: &[&str]) -> Option<Value> {
    slot_parser(name)?(words, None)
}

/// The intent whose template matches with the fewest ignored words; ties go
/// to the earlier template.
pub fn parse_at(text: &str, ctx: &Context) -> Option<Action> {
    let owned = normalize(text);
    let words: Vec<&str> = owned.iter().map(String::as_str).collect();
    best_match(&words, ctx, false).or_else(|| best_match(&words, ctx, true))
}

/// Like `parse_at`, but only the catch-alls (`fallback`) or only the rest.
pub fn parse_tier(text: &str, ctx: &Context, fallback: bool) -> Option<Action> {
    let owned = normalize(text);
    let words: Vec<&str> = owned.iter().map(String::as_str).collect();
    best_match(&words, ctx, fallback)
}

fn best_match(words: &[&str], ctx: &Context, fallback: bool) -> Option<Action> {
    INTENTS
        .iter()
        .filter(|i| i.fallback == fallback)
        .flat_map(|i| i.templates.iter().map(move |t| (i, t)))
        .filter_map(|(i, t)| {
            let (skipped, slots) = t.find(words, &slot_parser)?;
            Some((skipped, (i.build)(&slots, ctx)?))
        })
        .min_by_key(|(skipped, _)| *skipped)
        .map(|(_, action)| action)
}

/// Spoken names of the slots with a fixed set of values.
fn closed_values(slot: &str) -> Option<Vec<Vec<&'static str>>> {
    let split = |names: &[&'static str]| {
        names
            .iter()
            .map(|n| n.split(' ').collect())
            .collect::<Vec<Vec<&str>>>()
    };
    Some(match slot {
        "host" => ops::HOSTS
            .iter()
            .flat_map(|(n, _)| n.iter().map(|w| w.to_vec()))
            .collect(),
        "bt" => ops::BT_DEVICES
            .iter()
            .flat_map(|(n, _, _)| n.iter().map(|w| w.to_vec()))
            .collect(),
        "station" => media::STATIONS
            .iter()
            .flat_map(|(n, _, _)| n.iter().map(|w| w.to_vec()))
            .collect(),
        "agent" => chat::AGENTS
            .iter()
            .flat_map(|(n, _)| n.iter().map(|w| w.to_vec()))
            .collect(),
        "app" => split(&apps::names().collect::<Vec<_>>()),
        "list" => notes::LISTS.iter().flat_map(|(n, _)| split(n)).collect(),
        _ => return None,
    })
}

/// Questions and harmless requests. Opposites embed close together ("too
/// loud", "louder"), so volume, players, power and toggles stay grammar-only.
pub fn classifiable(a: &Action) -> bool {
    match a {
        Action::Chat(_)
        | Action::Fun(_)
        | Action::Info(_)
        | Action::Calc(_)
        | Action::News(_)
        | Action::Repeat => true,
        Action::Ops(o) => !matches!(o, ops::Ops::Bluetooth { .. }),
        Action::Media(m) => *m == media::Media::NowPlaying,
        Action::Timer(t) => matches!(t, timer::Timer::Left | timer::Timer::Start { .. }),
        Action::Meeting(m) => !matches!(
            m,
            meetings::Meeting::Join | meetings::Meeting::Create { .. }
        ),
        Action::Notes(n) => matches!(
            n,
            notes::Notes::Show(_) | notes::Notes::Contains { .. } | notes::Notes::Reminders
        ),
        _ => false,
    }
}

/// Sentences the classifier must not answer with `kind`, though they embed
/// close to it. "Add eggs and remove cheese" lands next to "is cheese on
/// the list", but showing the list would ignore what was asked.
pub fn vetoed(kind: &str, text: &str) -> bool {
    const CHANGE: &[&str] = &[
        "add", "put", "remove", "removed", "delete", "erase", "take", "forget", "cross",
    ];
    const QUESTION: &[&str] = &[
        "did", "have", "is", "are", "do", "does", "what", "what's", "which", "has",
    ];
    if !kind.starts_with("Notes(") {
        return false;
    }
    let words = crate::grammar::normalize(text);
    let question = words
        .first()
        .is_some_and(|w| QUESTION.contains(&w.as_str()));
    !question && words.iter().any(|w| CHANGE.contains(&w.as_str()))
}

/// A few values per slot for example sentences.
fn example_values(slot: &str) -> Option<Vec<String>> {
    const POOL: &[&str] = &[
        "five minutes",
        "ten minutes",
        "one hour",
        "7 pm",
        "half past six",
        "noon",
        "tomorrow",
        "today",
        "tomorrow morning",
        "this evening",
        "3",
        "20",
        "50",
        "munich",
        "berlin",
        "milk",
        "milk and eggs",
        "shopping",
        "firefox",
        "terminal",
        "two plus two",
        "something",
        "the dishes",
        "wifi",
    ];

    if let Some(v) = closed_values(slot) {
        return Some(v.iter().take(3).map(|w| w.join(" ")).collect());
    }
    let parse = slot_parser(slot)?;
    let ok: Vec<String> = POOL
        .iter()
        .filter(|p| parse(&p.split(' ').collect::<Vec<_>>(), None).is_some())
        .take(3)
        .map(|p| (*p).to_owned())
        .collect();
    (!ok.is_empty()).then_some(ok)
}

/// Grammar sentences for the intent classifier, and whether their template
/// has slots (then `fill` must find the values again).
pub fn classifier_examples() -> Vec<(String, Action, bool)> {
    let ctx = Context::local();
    let mut out: Vec<(String, Action, bool)> = INTENTS
        .iter()
        .skip(followup::intents().len())
        .filter(|i| !i.fallback)
        .flat_map(|i| &i.templates)
        .filter(|t| !t.has_open_slots())
        .flat_map(|t| {
            let slotted = t.has_slots();
            t.expand(&example_values, 40)
                .into_iter()
                .map(move |s| (s, slotted))
        })
        .filter_map(|(s, slotted)| {
            let a = parse_tier(&s, &ctx, false).filter(classifiable)?;
            Some((s, a, slotted))
        })
        .collect();
    out.sort_by(|a, b| a.0.cmp(&b.0));
    out.dedup_by(|a, b| a.0 == b.0);
    out
}

/// An action of `kind` with slot values found anywhere in `text`.
pub fn fill(text: &str, kind: &str, ctx: &Context) -> Option<Action> {
    let owned = normalize(text);
    let words: Vec<&str> = owned.iter().map(String::as_str).collect();
    INTENTS
        .iter()
        .filter(|i| !i.fallback)
        .flat_map(|i| i.templates.iter().map(move |t| (i, t)))
        .filter(|(_, t)| t.has_slots())
        .filter_map(|(i, t)| {
            let slots = t.slots_anywhere(&words, &slot_parser)?;
            let a = (i.build)(&slots, ctx).filter(classifiable)?;
            (crate::replay::kind(&a) == kind).then_some((slots.len(), a))
        })
        .max_by_key(|(n, _)| *n)
        .map(|(_, a)| a)
}

/// Every plain sentence with a named thing in it ("ping eve", "play
/// lassulus radio"), with its sound skeleton.
static NAMED: LazyLock<Vec<(String, Vec<char>)>> = LazyLock::new(|| {
    let mut all: Vec<String> = INTENTS
        .iter()
        .filter(|i| !i.fallback)
        .flat_map(|i| i.templates.iter().flat_map(|t| t.sentences(&closed_values)))
        .collect();
    all.sort();
    all.dedup();
    all.into_iter()
        .map(|s| {
            let k = fuzzy::skeleton(&s);
            (s, k)
        })
        .collect()
});

/// Commands that sound like `heard`, closest first, as whisper would
/// write them: candidates for rescoring a transcript nothing understood.
pub fn sound_alikes(heard: &str, limit: usize) -> Vec<String> {
    let h = fuzzy::skeleton(heard);
    if h.len() < 3 {
        return Vec::new();
    }
    let mut near: Vec<(usize, &str)> = NAMED
        .iter()
        .map(|(s, k)| (fuzzy::distance(&h, k), s.as_str()))
        // Half the sounds may differ; whisper mangles names, not everything.
        .filter(|(d, s)| *d * 2 <= h.len().max(s.len().min(h.len() * 2)))
        .collect();
    near.sort_by_key(|(d, _)| *d);
    near.into_iter()
        .take(limit)
        .map(|(_, s)| {
            let mut c = s.chars();
            c.next()
                .map(|f| format!("{}{}.", f.to_uppercase(), c.as_str()))
                .unwrap_or_default()
        })
        .collect()
}

/// What follows the wake word at the start of `heard` ("Alexa, play
/// music." -> "play music."), or `None` if it does not start with one.
pub fn strip_wake_word(heard: &str) -> Option<&str> {
    let target = fuzzy::skeleton("alexa");
    let mut rest = heard;
    for _ in 0..2 {
        let trimmed = rest.trim_start_matches(|c: char| !c.is_alphanumeric());
        let end = trimmed
            .find(|c: char| !c.is_alphanumeric() && c != '\'')
            .unwrap_or(trimmed.len());
        let (word, after) = trimmed.split_at(end);
        let after = after.trim_start_matches(|c: char| !c.is_alphanumeric());
        // "Alexis", "Alexei", "Elexa" are her too.
        if fuzzy::distance(&fuzzy::skeleton(word), &target) <= 1 {
            return Some(after);
        }
        if !["hey", "hi", "ok", "okay", "oh"].contains(&word.to_lowercase().as_str()) {
            return None;
        }
        rest = after;
    }
    None
}

/// Something to say right away for actions that wait on the network, so
/// the silence doesn't feel like a crash.
pub fn filler(action: &Action) -> Option<String> {
    Some(pick(action.skill()?.filler()?, ""))
}

/// Run an action and pick something to say about it.
pub fn run(action: &Action, ctx: &Context) -> Reply {
    if let Some(skill) = action.skill() {
        return skill.run(ctx);
    }
    match action {
        // Answered by the caller, which knows what it said last.
        Action::Repeat => Reply::new("", "", String::new()),
        Action::Voice(false) => Reply::new("Voice off", "", pick(SHUT_UP, "")),
        Action::Voice(true) => Reply::new("Voice on", "", pick(TALK_AGAIN, "")),
        Action::Say(text) => Reply::new("", "", text.clone()),
        Action::Listen(false) => Reply::new(
            "Assistant asleep",
            "Say \"Alexa, wake up\"",
            pick(ASLEEP, ""),
        ),
        Action::Listen(true) => Reply::new("Assistant listening", "", pick(AWAKE, "")),
        Action::Nap(secs) => {
            let what = timer::spoken_duration(*secs);
            let speech = pick(
                &[
                    "Nap time! See you in {}.",
                    "Ignoring you for {}. Bliss.",
                    "Back in {}. Don't miss me too much.",
                ],
                &what,
            );
            Reply::new("Assistant napping", &format!("for {what}"), speech)
        }
        Action::NapQuestion => {
            let mut r = Reply::new(
                "Nap?",
                "",
                pick(
                    &[
                        "No! I need one. Can I? Twenty minutes?",
                        "A nap? I wish. Can I have one? Please?",
                    ],
                    "",
                ),
            );
            r.follow_up = Some(Box::new(|text, _| match system::yes(text)? {
                true => Some(Action::Nap(20 * 60)),
                false => Some(Action::Say(
                    "Fine. I'll just suffer quietly. Loudly.".into(),
                )),
            }));
            r
        }
        Action::Which(op) => system::ask_which(*op),
        Action::Dismiss => Reply::new(
            "Never mind",
            "",
            choose(&[(ctx.late(), DISMISS_LATE)], DISMISS, ""),
        ), // Ran above; `Action::skill` lists every variant explicitly.
        _ => Reply::new("", "", String::new()),
    }
}

/// Lines from every case whose condition holds, so the situation shapes
/// the reply; the generic lines only when none applies.
#[cfg(test)]
mod tests {
    use super::*;

    pub const AT_14H: Context = Context::at(14 * 3600);

    /// `DUMP_EXAMPLES=f.tsv cargo test dump_examples -- --ignored` writes
    /// `kind \t sentence` for every intent, as training data for a classifier.
    #[test]
    #[ignore]
    fn dump_examples() {
        let skip = followup::intents().len();
        let mut out = String::new();
        for i in INTENTS.iter().skip(skip).filter(|i| !i.fallback) {
            for t in &i.templates {
                for s in t.expand(&example_values, 40) {
                    // The grammar labels its own sentences; unparsable
                    // samples (a slot value that needs context) are dropped.
                    if let Some(a) = parse_at(&s, &AT_14H) {
                        out.push_str(&format!("{}\t{s}\n", crate::replay::kind(&a)));
                    }
                }
            }
        }
        let path = std::env::var("DUMP_EXAMPLES").expect("DUMP_EXAMPLES");
        std::fs::write(path, out).expect("write");
    }

    /// `SOUND_ALIKES=in.txt cargo test dump_sound_alikes -- --ignored`
    /// prints the rescore candidates of every line, tab separated, so other
    /// recognisers can be tried on the same choices.
    #[test]
    #[ignore]
    fn dump_sound_alikes() {
        let path = std::env::var("SOUND_ALIKES").expect("SOUND_ALIKES");
        for heard in std::fs::read_to_string(path).expect("read").lines() {
            println!("{heard}\t{}", sound_alikes(heard, 8).join("\t"));
        }
    }

    #[test]
    fn wake_word_is_stripped() {
        assert_eq!(strip_wake_word("Alexa, play music."), Some("play music."));
        assert_eq!(strip_wake_word("Alexa play music"), Some("play music"));
        assert_eq!(
            strip_wake_word("Hey Alexa. What time is it?"),
            Some("What time is it?")
        );
        assert_eq!(strip_wake_word("Alexis, stop."), Some("stop."));
        assert_eq!(strip_wake_word("Alexa?"), Some(""));
        assert_eq!(strip_wake_word("That makes sense. Now's the time."), None);
        assert_eq!(strip_wake_word("All right, all right."), None);
        assert_eq!(strip_wake_word(""), None);
    }

    #[test]
    fn sound_alikes_for_mangled_names() {
        let c = sound_alikes("PingYiF.", 8);
        assert!(c.iter().any(|s| s == "Ping eve."), "{c:?}");
        assert!(c.iter().any(|s| s == "Ping eva."), "{c:?}");
        let c = sound_alikes("Play Laslos Radio.", 5);
        assert!(c.iter().any(|s| s.contains("lassulus")), "{c:?}");
        assert!(sound_alikes("ok", 5).is_empty());
    }

    #[test]
    fn all_templates_parse() {
        for i in INTENTS.iter() {
            assert_eq!(
                i.templates.len(),
                i.declared,
                "a template failed to parse; see stderr"
            );
        }
    }

    #[test]
    fn list_changes_are_vetoed_for_notes() {
        assert!(vetoed(
            "Notes(Show",
            "add eggs to my list and remove cheese"
        ));
        assert!(vetoed("Notes(Show", "take carrots off my list"));
        assert!(!vetoed("Notes(Show", "did i put oatmeal on my list"));
        assert!(!vetoed("Notes(Show", "what's on my shopping list"));
        assert!(!vetoed("Fun(Joke)", "take a joke"));
    }

    #[test]
    fn pick_never_repeats() {
        let lines = ["a {}", "b {}"];
        let first = pick(&lines, "x");
        assert!(first.ends_with(" x"));
        for _ in 0..10 {
            assert_ne!(pick(&lines, "x"), pick(&lines, "x"));
        }
    }

    #[test]
    fn not_understood_quotes() {
        let r = not_understood("make me a sandwich.", 1);
        assert!(r.to_lowercase().contains("make me a sandwich"), "{r}");
        assert!(r.starts_with(char::is_uppercase), "{r}");
        assert!(!not_understood("", 1).is_empty());
        let long = "word ".repeat(20);
        assert!(!not_understood(&long, 1).contains("word"));
    }

    #[test]
    fn voice() {
        for t in ["Shut up!", "Alexa, be quiet.", "Stop talking.", "Zip it."] {
            assert_eq!(parse_at(t, &AT_14H), Some(Action::Voice(false)), "{t:?}");
        }
        for t in ["You can talk again.", "Talk to me."] {
            assert_eq!(parse_at(t, &AT_14H), Some(Action::Voice(true)), "{t:?}");
        }
    }

    #[test]
    fn choose_prefers_matching_case() {
        for _ in 0..10 {
            assert_eq!(
                choose(&[(false, &["no"]), (true, &["yes {}"])], &["generic"], "x"),
                "yes x"
            );
            assert_eq!(choose(&[(false, &["no"])], &["generic"], ""), "generic");
        }
    }

    #[test]
    fn patience_runs_out() {
        let first = not_understood("bake a cake", 1);
        assert!(
            say::NOT_UNDERSTOOD
                .iter()
                .any(|l| l.replace("{}", "bake a cake").eq_ignore_ascii_case(&first)),
            "{first}"
        );
        let third = not_understood("bake a cake", 3);
        assert!(
            say::GIVE_UP
                .iter()
                .any(|l| l.replace("{}", "bake a cake").eq_ignore_ascii_case(&third)),
            "{third}"
        );
    }

    #[test]
    fn assistant_sleep() {
        for t in [
            "Shut yourself down.",
            "Stop listening.",
            "Take a break.",
            "Alexa, go away.",
        ] {
            assert_eq!(parse_at(t, &AT_14H), Some(Action::Listen(false)), "{t:?}");
        }
        for t in ["Wake up!", "Start listening.", "Come back."] {
            assert_eq!(parse_at(t, &AT_14H), Some(Action::Listen(true)), "{t:?}");
        }
    }

    #[test]
    fn naps() {
        for (t, secs) in [
            ("Take a break for 20 minutes.", 1200),
            ("Be quiet for an hour", 3600),
            ("Leave me alone for 10 minutes", 600),
            ("Stop listening for half an hour", 1800),
        ] {
            assert_eq!(parse_at(t, &AT_14H), Some(Action::Nap(secs)), "{t:?}");
        }
        assert_eq!(
            parse_at("Did you have a nap today?", &AT_14H),
            Some(Action::NapQuestion)
        );
        let follow = run(&Action::NapQuestion, &AT_14H).follow_up.expect("asks");
        assert_eq!(follow("Yes.", &AT_14H), Some(Action::Nap(1200)));
        let chatter = "I'm saying something else. Please. I'll take one, please.";
        assert_eq!(system::yes(chatter), None);
    }

    #[test]
    fn dismiss() {
        for text in ["Never mind.", "Nevermind", "Alexa, forget it.", "Nothing."] {
            assert_eq!(parse_at(text, &AT_14H), Some(Action::Dismiss), "{text:?}");
        }
    }

    /// `cargo test --release -- --ignored --nocapture bench`
    #[test]
    #[ignore = "benchmark"]
    fn bench() {
        use std::hint::black_box;
        use std::time::Instant;
        let inputs = [
            "Set a timer for 5 minutes.",
            "Alexa, could you please set a timer for two hours and fifteen minutes for me",
            "Set an alarm for half past seven in the evening",
            "Cancel the timer.",
            "The timer is ready.",
            "What is the weather like tomorrow in Munich?",
            "Thank you.",
        ];
        for text in inputs {
            let _ = parse_at(text, &AT_14H); // warm up LazyLock
            let iters = 20_000u32;
            let t = Instant::now();
            for _ in 0..iters {
                black_box(parse_at(black_box(text), &AT_14H));
            }
            let per = t.elapsed() / iters;
            println!("{per:>10.2?}  {text:?} -> {:?}", parse_at(text, &AT_14H));
        }
    }
}
