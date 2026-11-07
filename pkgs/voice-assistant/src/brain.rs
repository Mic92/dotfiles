//! Client for the Lisp brain: one JSON object per line over a Unix socket,
//! each reply carrying the request's id. See brain/src/protocol.lisp.

use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::os::unix::net::UnixStream;
use std::path::PathBuf;
use std::process::{self, Child, Command, Stdio};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex, OnceLock, PoisonError};
use std::thread;
use std::time::{Duration, Instant};

use tinyjson::JsonValue;

use crate::classify::Nearest;
use crate::clock::Clock;
use crate::rescore::SoundAlikes;

pub const VERSION: f64 = 1.0;

/// What the brain makes of one text, as Rust's `Debug` prints actions.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Parses {
    pub strict: Option<String>,
    pub fallback: Option<String>,
}

pub struct Brain {
    reader: BufReader<UnixStream>,
    writer: UnixStream,
    next_id: u64,
    /// A brain started just for us, killed on drop.
    private: Option<(Child, PathBuf)>,
}

fn live_socket() -> PathBuf {
    let dir = env::var_os("XDG_RUNTIME_DIR").map_or_else(|| "/tmp".into(), PathBuf::from);
    dir.join("voice-assistant/brain.sock")
}

fn brain_program() -> String {
    env::var("VA_BRAIN")
        .ok()
        .or_else(|| option_env!("VA_BRAIN").map(String::from))
        .unwrap_or_else(|| "va-brain".into())
}

impl Brain {
    /// The running brain, or else a private one on a temporary socket, so
    /// `--parse` and CI work without the service.
    pub fn connect() -> Result<Self, String> {
        if let Ok(s) = UnixStream::connect(live_socket()) {
            return Self::over(s);
        }
        static CONNECTIONS: AtomicU32 = AtomicU32::new(0);
        let n = CONNECTIONS.fetch_add(1, Ordering::Relaxed);
        let sock = env::temp_dir().join(format!("va-brain-{}-{n}.sock", process::id()));
        let mut child = Command::new(brain_program())
            .arg("--socket")
            .arg(&sock)
            .stdout(Stdio::null())
            .spawn()
            .map_err(|e| format!("starting {}: {e}", brain_program()))?;
        let deadline = Instant::now() + Duration::from_secs(30);
        loop {
            if let Ok(s) = UnixStream::connect(&sock) {
                let mut b = Self::over(s)?;
                b.private = Some((child, sock));
                return Ok(b);
            }
            if let Ok(Some(status)) = child.try_wait() {
                return Err(format!("the brain exited: {status}"));
            }
            if Instant::now() > deadline {
                let _ = child.kill();
                return Err("the brain did not open its socket in 30 s".into());
            }
            thread::sleep(Duration::from_millis(50));
        }
    }

    /// Greets the brain on STREAM and checks it speaks our version.
    pub fn over(stream: UnixStream) -> Result<Self, String> {
        let writer = stream.try_clone().map_err(|e| e.to_string())?;
        let mut b = Brain {
            reader: BufReader::new(stream),
            writer,
            next_id: 1,
            private: None,
        };
        let reply = b.request("hello", HashMap::from([("version".into(), VERSION.into())]))?;
        match reply.get("version") {
            Some(JsonValue::Number(v)) if *v == VERSION => Ok(b),
            v => Err(format!(
                "the brain speaks version {v:?}, we speak {VERSION}"
            )),
        }
    }

    fn request(
        &mut self,
        kind: &str,
        fields: HashMap<String, JsonValue>,
    ) -> Result<HashMap<String, JsonValue>, String> {
        self.request_with(kind, fields, |_| {})
    }

    /// Sends a request and waits for its reply, handing anything the brain
    /// sends first, such as fillers, to `early`.
    fn request_with(
        &mut self,
        kind: &str,
        mut fields: HashMap<String, JsonValue>,
        mut early: impl FnMut(&HashMap<String, JsonValue>),
    ) -> Result<HashMap<String, JsonValue>, String> {
        let id = self.next_id;
        self.next_id += 1;
        fields.insert("id".into(), (id as f64).into());
        fields.insert("type".into(), kind.to_string().into());
        let line = JsonValue::from(fields)
            .stringify()
            .map_err(|e| e.to_string())?;
        writeln!(self.writer, "{line}").map_err(|e| format!("writing to the brain: {e}"))?;
        loop {
            let mut answer = String::new();
            match self.reader.read_line(&mut answer) {
                Ok(0) => return Err("the brain hung up".into()),
                Err(e) => return Err(format!("reading from the brain: {e}")),
                Ok(_) => {}
            }
            let reply = decode(&answer)?;
            match reply.get("id") {
                Some(JsonValue::Number(n)) if *n == id as f64 => {}
                other => return Err(format!("reply for id {other:?}, expected {id}")),
            }
            let reply_kind = if kind == "parse" { "parsed" } else { kind };
            if reply.get("type") == Some(&JsonValue::String(reply_kind.into())) {
                return Ok(reply);
            }
            early(&reply);
        }
    }

    /// What the brain makes of an utterance. Fillers go to `filler` while
    /// the command runs.
    pub fn heard(&mut self, h: &Hearing, mut filler: impl FnMut(String)) -> Result<Heard, String> {
        let reply = self.request_with("heard", h.fields(), |m| {
            if let Some(JsonValue::String(s)) = m.get("speech") {
                filler(s.clone());
            }
        })?;
        Heard::from(&reply)
    }

    /// (text, kind) for the classifier to learn from.
    pub fn examples(&mut self, ctx: &Clock) -> Result<Vec<(String, String)>, String> {
        let reply = self.request("examples", context_fields(ctx))?;
        let Some(JsonValue::Array(a)) = reply.get("examples") else {
            return Err("examples: no examples".into());
        };
        a.iter()
            .map(|e| {
                let field = |k: &str| match e {
                    JsonValue::Object(o) => match o.get(k) {
                        Some(JsonValue::String(s)) => Some(s.clone()),
                        _ => None,
                    },
                    _ => None,
                };
                field("text")
                    .zip(field("kind"))
                    .ok_or_else(|| format!("examples: {e:?}"))
            })
            .collect()
    }

    /// The action the classifier's guess stands for, and whether to take it.
    pub fn classified(
        &mut self,
        text: &str,
        n: &Nearest,
        ctx: &Clock,
    ) -> Result<(Option<String>, bool), String> {
        let mut fields = context_fields(ctx);
        fields.extend(guess(n));
        fields.insert("text".into(), text.to_owned().into());
        let reply = self.request("classified", fields)?;
        let action = match reply.get("action") {
            Some(JsonValue::String(s)) => Some(s.clone()),
            _ => None,
        };
        Ok((
            action,
            reply.get("accepted") == Some(&JsonValue::Boolean(true)),
        ))
    }

    pub fn lexicon(&mut self) -> Result<Vec<String>, String> {
        match self.request("lexicon", HashMap::new())?.get("sentences") {
            Some(JsonValue::Array(a)) => a
                .iter()
                .map(|v| match v {
                    JsonValue::String(s) => Ok(s.clone()),
                    v => Err(format!("lexicon: {v:?}")),
                })
                .collect(),
            v => Err(format!("lexicon: sentences is {v:?}")),
        }
    }

    pub fn parse(&mut self, texts: &[String], ctx: &Clock) -> Result<Vec<Parses>, String> {
        let reply = self.request("parse", parse_fields(texts, ctx))?;
        parses(&reply)
    }
}

impl Drop for Brain {
    fn drop(&mut self) {
        if let Some((child, sock)) = &mut self.private {
            let _ = child.kill();
            let _ = child.wait();
            let _ = fs::remove_file(sock);
        }
    }
}

/// One utterance and what the brain needs to know around it.
pub struct Hearing {
    pub text: String,
    pub now: u32,
    pub today: i64,
    pub music: bool,
    /// We were listening for the answer to the brain's question.
    pub answering: bool,
    /// Listening ran into the time limit: only exact commands count.
    pub cut_off: bool,
    /// Only parse, as for `--stdin`.
    pub dry: bool,
    /// Answer "unsure" instead of trying the catch-alls, to rescore first.
    pub unsure: bool,
    /// The classifier's nearest example, tried before the catch-alls.
    pub nearest: Option<Nearest>,
}

impl Hearing {
    fn fields(&self) -> HashMap<String, JsonValue> {
        HashMap::from([
            ("text".into(), self.text.clone().into()),
            ("now".into(), f64::from(self.now).into()),
            ("today".into(), (self.today as f64).into()),
            ("music".into(), self.music.into()),
            ("answering".into(), self.answering.into()),
            ("cut_off".into(), self.cut_off.into()),
            ("dry".into(), self.dry.into()),
            ("unsure".into(), self.unsure.into()),
            (
                "classified".into(),
                self.nearest.as_ref().map_or(JsonValue::Null, |n| {
                    JsonValue::from(guess(n).into_iter().collect::<HashMap<_, _>>())
                }),
            ),
        ])
    }
}

fn guess(n: &Nearest) -> [(String, JsonValue); 3] {
    [
        ("kind".into(), n.intent.clone().into()),
        ("score".into(), f64::from(n.score).into()),
        ("lead".into(), f64::from(n.lead).into()),
    ]
}

fn context_fields(ctx: &Clock) -> HashMap<String, JsonValue> {
    HashMap::from([
        ("now".into(), f64::from(ctx.now).into()),
        ("today".into(), (ctx.today as f64).into()),
    ])
}

/// What to do about an utterance.
#[derive(Debug, Default, PartialEq)]
pub struct Heard {
    /// command, miss, noise or asleep, as the wake log records it.
    pub outcome: String,
    pub action: Option<String>,
    /// What heard.tsv logs, see `replay::kind`.
    pub kind: Option<String>,
    pub title: String,
    pub body: String,
    pub speech: String,
    pub cache: bool,
    pub chime: Option<String>,
    pub notify: bool,
    /// Turn the voice on before speaking or off after it.
    pub voice: Option<bool>,
    pub leave_paused: bool,
    /// Listen for an answer without the wake word.
    pub question: bool,
}

impl Heard {
    fn from(o: &HashMap<String, JsonValue>) -> Result<Self, String> {
        let text = |k: &str| match o.get(k) {
            Some(JsonValue::String(s)) => Ok(Some(s.clone())),
            Some(JsonValue::Null) => Ok(None),
            v => Err(format!("heard: {k} is {v:?}")),
        };
        let flag = |k: &str| match o.get(k) {
            Some(JsonValue::Boolean(b)) => Ok(Some(*b)),
            Some(JsonValue::Null) => Ok(None),
            v => Err(format!("heard: {k} is {v:?}")),
        };
        Ok(Heard {
            outcome: text("outcome")?.unwrap_or_default(),
            action: text("action")?,
            kind: text("kind")?,
            title: text("title")?.unwrap_or_default(),
            body: text("body")?.unwrap_or_default(),
            speech: text("speech")?.unwrap_or_default(),
            cache: flag("cache")?.unwrap_or(false),
            chime: text("chime")?,
            notify: flag("notify")?.unwrap_or(false),
            voice: flag("voice")?,
            leave_paused: flag("leave_paused")?.unwrap_or(false),
            question: flag("question")?.unwrap_or(false),
        })
    }
}

/// The brain behind a lock, shared with the threads running commands, and
/// reconnected when it went away, as after a restart.
#[derive(Clone)]
pub struct Shared {
    brain: Arc<Mutex<Option<Brain>>>,
    alikes: Arc<OnceLock<SoundAlikes>>,
}

impl Shared {
    pub fn new(b: Brain) -> Self {
        Shared {
            brain: Arc::new(Mutex::new(Some(b))),
            alikes: Arc::new(OnceLock::new()),
        }
    }

    pub fn heard(&self, h: &Hearing, mut filler: impl FnMut(String)) -> Result<Heard, String> {
        let mut guard = self.brain.lock().unwrap_or_else(PoisonError::into_inner);
        if let Some(b) = guard.as_mut() {
            match b.heard(h, &mut filler) {
                Ok(r) => return Ok(r),
                Err(e) => eprintln!("voice-assistant: {e}, reconnecting"),
            }
        }
        *guard = None;
        let b = guard.insert(Brain::connect()?);
        b.heard(h, filler)
    }

    /// Both readings of each text, nothing when the brain is away.
    pub fn parses(&self, texts: &[String], ctx: &Clock) -> Vec<Parses> {
        let mut guard = self.brain.lock().unwrap_or_else(PoisonError::into_inner);
        let parsed = match guard.as_mut() {
            Some(b) => b.parse(texts, ctx),
            None => Err("not connected".into()),
        };
        parsed
            .inspect_err(|e| eprintln!("voice-assistant: {e}"))
            .unwrap_or_else(|_| texts.iter().map(|_| Parses::default()).collect())
    }

    /// The brain's commands to search for sound-alikes, fetched once.
    pub fn sound_alikes(&self) -> &SoundAlikes {
        self.alikes.get_or_init(|| {
            let mut guard = self.brain.lock().unwrap_or_else(PoisonError::into_inner);
            let sentences = match guard.as_mut() {
                Some(b) => b.lexicon(),
                None => Err("not connected".into()),
            };
            SoundAlikes::new(
                sentences
                    .inspect_err(|e| eprintln!("voice-assistant: lexicon: {e}"))
                    .unwrap_or_default(),
            )
        })
    }

    fn with<T>(&self, f: impl FnOnce(&mut Brain) -> Result<T, String>) -> Result<T, String> {
        let mut guard = self.brain.lock().unwrap_or_else(PoisonError::into_inner);
        match guard.as_mut() {
            Some(b) => f(b),
            None => Err("not connected".into()),
        }
    }

    pub fn examples(&self, ctx: &Clock) -> Result<Vec<(String, String)>, String> {
        self.with(|b| b.examples(ctx))
    }

    pub fn classified(
        &self,
        text: &str,
        n: &Nearest,
        ctx: &Clock,
    ) -> Result<(Option<String>, bool), String> {
        self.with(|b| b.classified(text, n, ctx))
    }

    /// The strict reading of each text.
    pub fn parse(&self, texts: &[String], ctx: &Clock) -> Vec<Option<String>> {
        self.parses(texts, ctx)
            .into_iter()
            .map(|p| p.strict)
            .collect()
    }
}

/// A reply line as an object, with error replies turned into `Err`.
fn decode(line: &str) -> Result<HashMap<String, JsonValue>, String> {
    let v: JsonValue = line
        .parse()
        .map_err(|e| format!("bad reply {line:?}: {e}"))?;
    let JsonValue::Object(o) = v else {
        return Err(format!("reply is not an object: {line:?}"));
    };
    if o.get("type") == Some(&JsonValue::String("error".into())) {
        return Err(match o.get("message") {
            Some(JsonValue::String(m)) => format!("the brain says: {m}"),
            _ => "the brain says: error".into(),
        });
    }
    Ok(o)
}

fn parse_fields(texts: &[String], ctx: &Clock) -> HashMap<String, JsonValue> {
    HashMap::from([
        ("now".into(), f64::from(ctx.now).into()),
        ("today".into(), (ctx.today as f64).into()),
        (
            "texts".into(),
            JsonValue::Array(texts.iter().map(|t| t.clone().into()).collect()),
        ),
    ])
}

fn parses(reply: &HashMap<String, JsonValue>) -> Result<Vec<Parses>, String> {
    let Some(JsonValue::Array(items)) = reply.get("parses") else {
        return Err("parsed without parses".into());
    };
    let shown = |o: &HashMap<String, JsonValue>, k| match o.get(k) {
        Some(JsonValue::String(s)) => Ok(Some(s.clone())),
        Some(JsonValue::Null) => Ok(None),
        v => Err(format!("{k} is {v:?}")),
    };
    items
        .iter()
        .map(|i| match i {
            JsonValue::Object(o) => Ok(Parses {
                strict: shown(o, "strict")?,
                fallback: shown(o, "fallback")?,
            }),
            v => Err(format!("a parse is {v:?}")),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::net::UnixListener;

    fn fixture(name: &str) -> HashMap<String, JsonValue> {
        let path = format!("{}/tests/protocol/{name}.json", env!("CARGO_MANIFEST_DIR"));
        let raw = fs::read_to_string(path).unwrap_or_default();
        match raw.parse() {
            Ok(JsonValue::Object(o)) => o,
            _ => HashMap::new(),
        }
    }

    fn part(f: &HashMap<String, JsonValue>, k: &str) -> HashMap<String, JsonValue> {
        match f.get(k) {
            Some(JsonValue::Object(o)) => o.clone(),
            _ => HashMap::new(),
        }
    }

    fn texts(req: &HashMap<String, JsonValue>) -> Vec<String> {
        match req.get("texts") {
            Some(JsonValue::Array(a)) => a
                .iter()
                .filter_map(|t| t.get::<String>().cloned())
                .collect(),
            _ => Vec::new(),
        }
    }

    #[test]
    fn requests_match_the_fixtures() {
        let req = part(&fixture("parse"), "request");
        let mut fields = parse_fields(&texts(&req), &Clock::at(9 * 3600));
        for k in ["id", "type"] {
            fields.insert(k.into(), req.get(k).cloned().unwrap_or(JsonValue::Null));
        }
        assert_eq!(fields, req);
    }

    #[test]
    fn replies_decode() {
        let reply = part(&fixture("parse"), "reply");
        let want = vec![
            Parses {
                strict: Some("Fun(Joke)".into()),
                fallback: None,
            },
            Parses {
                strict: None,
                fallback: None,
            },
            Parses {
                strict: Some("Fun(Dice(6))".into()),
                fallback: None,
            },
        ];
        assert_eq!(parses(&reply), Ok(want));
        let error = JsonValue::from(part(&fixture("unknown"), "reply"));
        let line = error.stringify().unwrap_or_default();
        assert_eq!(
            decode(&line),
            Err("the brain says: unknown message type \"frobnicate\"".into())
        );
    }

    #[test]
    fn heard_fixture() {
        let f = fixture("heard");
        let req = part(&f, "request");
        let h = Hearing {
            text: "roll a die".into(),
            now: 36000,
            today: 20383,
            music: false,
            answering: false,
            cut_off: false,
            dry: true,
            unsure: false,
            nearest: None,
        };
        let mut fields = h.fields();
        for k in ["id", "type"] {
            fields.insert(k.into(), req.get(k).cloned().unwrap_or(JsonValue::Null));
        }
        assert_eq!(fields, req);
        let got = Heard::from(&part(&f, "reply"));
        assert_eq!(
            got,
            Ok(Heard {
                outcome: "command".into(),
                action: Some("Fun(Dice(6))".into()),
                kind: Some("Fun(Dice".into()),
                ..Heard::default()
            })
        );
    }

    /// A fake brain answering each line with the next canned reply.
    #[test]
    fn talks_to_a_brain() {
        let dir = env::temp_dir().join(format!("va-brain-test-{}", process::id()));
        let _ = fs::create_dir_all(&dir);
        let sock = dir.join("brain.sock");
        let _ = fs::remove_file(&sock);
        let Ok(listener) = UnixListener::bind(&sock) else {
            panic!("bind {}", sock.display());
        };
        let replies: Vec<String> = ["hello", "parse"]
            .iter()
            .map(|n| JsonValue::from(part(&fixture(n), "reply")))
            .filter_map(|v| v.stringify().ok())
            .collect();
        let server = thread::spawn(move || {
            let Ok((s, _)) = listener.accept() else {
                return;
            };
            let mut w = s.try_clone().ok();
            let mut lines = BufReader::new(s).lines();
            for r in replies {
                if lines.next().is_none() {
                    return;
                }
                if let Some(w) = &mut w {
                    let _ = writeln!(w, "{r}");
                }
            }
        });
        let Ok(s) = UnixStream::connect(&sock) else {
            panic!("connect");
        };
        let mut b = Brain::over(s).unwrap_or_else(|e| panic!("{e}"));
        b.next_id = 2;
        let got = b.parse(&["tell me a joke".into()], &Clock::at(9 * 3600));
        assert_eq!(got.map(|p| p.len()), Ok(3));
        let _ = server.join();
        let _ = fs::remove_dir_all(&dir);
    }
}
