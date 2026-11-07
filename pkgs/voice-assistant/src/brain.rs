//! Client for the Lisp brain: one JSON object per line over a Unix socket,
//! each reply carrying the request's id. See brain/src/protocol.lisp.

use std::collections::HashMap;
use std::env;
use std::io::{BufRead, BufReader, Write};
use std::os::unix::net::UnixStream;
use std::path::PathBuf;
use std::process::{Child, Command};
use std::thread;
use std::time::{Duration, Instant};

use tinyjson::JsonValue;

use crate::skills::Context;

pub const VERSION: f64 = 1.0;

/// What the brain makes of one text, as Rust's `Debug` prints actions.
#[derive(Debug, PartialEq, Eq)]
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
        let sock = env::temp_dir().join(format!("va-brain-{}.sock", std::process::id()));
        let mut child = Command::new(brain_program())
            .arg("--socket")
            .arg(&sock)
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
        mut fields: HashMap<String, JsonValue>,
    ) -> Result<HashMap<String, JsonValue>, String> {
        let id = self.next_id;
        self.next_id += 1;
        fields.insert("id".into(), (id as f64).into());
        fields.insert("type".into(), kind.to_string().into());
        let line = JsonValue::from(fields)
            .stringify()
            .map_err(|e| e.to_string())?;
        writeln!(self.writer, "{line}").map_err(|e| format!("writing to the brain: {e}"))?;
        let mut answer = String::new();
        match self.reader.read_line(&mut answer) {
            Ok(0) => return Err("the brain hung up".into()),
            Err(e) => return Err(format!("reading from the brain: {e}")),
            Ok(_) => {}
        }
        let reply = decode(&answer)?;
        match reply.get("id") {
            Some(JsonValue::Number(n)) if *n == id as f64 => Ok(reply),
            other => Err(format!("reply for id {other:?}, expected {id}")),
        }
    }

    pub fn parse(&mut self, texts: &[String], ctx: &Context) -> Result<Vec<Parses>, String> {
        let reply = self.request("parse", parse_fields(texts, ctx))?;
        parses(&reply)
    }
}

impl Drop for Brain {
    fn drop(&mut self) {
        if let Some((child, sock)) = &mut self.private {
            let _ = child.kill();
            let _ = child.wait();
            let _ = std::fs::remove_file(sock);
        }
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

fn parse_fields(texts: &[String], ctx: &Context) -> HashMap<String, JsonValue> {
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
        let raw = std::fs::read_to_string(path).unwrap_or_default();
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
        let mut fields = parse_fields(&texts(&req), &Context::at(9 * 3600));
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

    /// A fake brain answering each line with the next canned reply.
    #[test]
    fn talks_to_a_brain() {
        let dir = env::temp_dir().join(format!("va-brain-test-{}", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);
        let sock = dir.join("brain.sock");
        let _ = std::fs::remove_file(&sock);
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
        let got = b.parse(&["tell me a joke".into()], &Context::at(9 * 3600));
        assert_eq!(got.map(|p| p.len()), Ok(3));
        let _ = server.join();
        let _ = std::fs::remove_dir_all(&dir);
    }
}
