//! Media players (MPRIS) via noctalia.

use super::fuzzy;
use super::succeeds;
use super::{Action, Context, Intent, Module, Reply, Skill, intent, noctalia, output, pick, spawn};
use crate::grammar::Value;
use std::env;
use std::os::unix::net::UnixStream;
use std::path::PathBuf;
use std::time::Duration;

#[derive(Debug, Clone, PartialEq)]
pub enum Media {
    Play,
    Pause,
    Next,
    Previous,
    Stop,
    /// Index into `STATIONS`.
    Radio(usize),
    RadioOff,
    NowPlaying,
    /// "Play Hey Jude": the first YouTube hit.
    Search(String),
}

const WHAT: &str = "[the|my|this|some] (music|song|track|video|podcast|media|playback)";

fn m(x: Media) -> Option<Action> {
    Some(Action::Media(x))
}

pub const MODULE: Module = Module {
    intents,
    slots: &[("station", |w, _| station_lookup(w).map(Value::Index))],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            &[
                &format!("(play|resume|continue|start) {WHAT}"),
                "!play",
                "!play something",
                "!(can|could|may) i (hear|listen to) [some|my] music",
                "!(i want to|i'd like to|let's) (hear|listen to) [some|my] music",
                "!put [on] [some|my] music [on] [please]",
            ],
            |_, _| m(Media::Play),
        ),
        intent(&[&format!("pause {WHAT}")], |_, _| m(Media::Pause)),
        intent(&[&format!("stop {WHAT}")], |_, _| m(Media::Stop)),
        intent(
            &[
                "[play] [the] next (song|track|one|video|episode)",
                "skip [this|the] [song|track|one]",
                "!next",
            ],
            |_, _| m(Media::Next),
        ),
        intent(
            &[
                "[play] [the] (previous|last) (song|track|one|video|episode)",
                "go back [a|one] (song|track)",
                "!previous",
            ],
            |_, _| m(Media::Previous),
        ),
        intent(
            &[
                "!(play|put on|turn on|start|listen to|switch to|tune in to|tune into) [the|some] {station} [radio] [please]",
                "!(play|put on|turn on|start) [the|some] radio {station}",
                "!i want to (listen to|hear) [the|some] {station}",
            ],
            |s, _| match s.get("station")? {
                Value::Index(i) => m(Media::Radio(*i)),
                _ => None,
            },
        ),
        intent(
            &[
                "!(play|put on|turn on|start) [the|some] radio",
                "!radio [please]",
            ],
            // The rhasspy days played the BBC by default.
            |_, _| m(Media::Radio(3)),
        ),
        intent(
            &[
                "!(stop|turn off|switch off|kill|enough) [the|that] radio",
                "!radio off",
            ],
            |_, _| m(Media::RadioOff),
        ),
        intent(
            &[
                "!(play|put on|listen to) {text}",
                "!put [on] {text} on",
                "!(i|we) (want|would like|'d like|need) to (listen to|hear|play) {text}",
                "!(can|could|may) (i|we) (hear|listen to|get) {text}",
                "!(let's|let me) (hear|listen to) {text}",
            ],
            |s, _| match s.get("text")? {
                Value::Text(t) => m(Media::Search(search_query(t)?)),
                _ => None,
            },
        ),
        // "turn on" alone is a light or a setting.
        intent(
            &[
                "!turn on [the|my] {text} playlist",
                "!turn [the|my] {text} playlist on",
                "!(start|resume|pull up|use|do) [playing] [the|my] {text} playlist [please]",
            ],
            |s, _| match s.get("text")? {
                Value::Text(t) => m(Media::Search(search_query(&format!("{t} playlist"))?)),
                _ => None,
            },
        ),
        intent(
            &[
                "!(what's|what is) (playing|this song|this track|the song|the track|on)",
                "!what (song|track|music) is (this|playing|that)",
                "!who (is|sings|plays) (this|that) [song|track]",
                "!(name|what's) (this|that) (song|tune)",
            ],
            |_, _| m(Media::NowPlaying),
        ),
    ]
}

const PLAY: &[&str] = &[
    "Music, maestro!",
    "Let's get this party started.",
    "Ooh, I love this one.",
    "Play it loud. The neighbours love you.",
    "Hit it!",
];
const PAUSE: &[&str] = &[
    "Paused. The silence is deafening.",
    "Holding the beat.",
    "Music paused. Dramatic pause.",
    "Paused. Go on, take that call.",
];
const NEXT: &[&str] = &[
    "Skip! Didn't like that one either.",
    "Next. You're picky today.",
    "Moving on.",
    "Next. That song was a crime anyway.",
    "Skipping. Your taste is evolving. Slowly.",
];
const PREVIOUS: &[&str] = &[
    "Going back. Nostalgic, are we?",
    "One more time, with feeling.",
    "Rewind!",
    "Back we go. Clearly a banger.",
];
const STOP: &[&str] = &[
    "Music stopped. Party's over.",
    "Stopped. Who killed the vibe? You did.",
    "Silence. Finally, I can hear myself think.",
];

/// Spoken names and stream URLs.
pub const STATIONS: &[(&[&[&str]], &str, &str)] = &[
    (
        &[
            &["radio", "1"],
            &["radio", "one"],
            &["bbc", "radio", "1"],
            &["bbc", "radio", "one"],
            &["bbc", "1"],
            &["bbc", "one"],
        ],
        "BBC Radio 1",
        "http://as-hls-ww-live.akamaized.net/pool_01505109/live/ww/bbc_radio_one/bbc_radio_one.isml/bbc_radio_one-audio%3d96000.norewind.m3u8",
    ),
    (
        &[
            &["radio", "4"],
            &["radio", "four"],
            &["bbc", "radio", "4"],
            &["bbc", "radio", "four"],
            &["bbc", "4"],
            &["bbc", "four"],
        ],
        "BBC Radio 4",
        "http://as-hls-ww-live.akamaized.net/pool_55057080/live/ww/bbc_radio_fourfm/bbc_radio_fourfm.isml/bbc_radio_fourfm-audio%3d96000.norewind.m3u8",
    ),
    (
        &[
            &["6", "music"],
            &["six", "music"],
            &["bbc", "6", "music"],
            &["bbc", "six", "music"],
        ],
        "BBC 6 Music",
        "http://as-hls-ww-live.akamaized.net/pool_81827798/live/ww/bbc_6music/bbc_6music.isml/bbc_6music-audio%3d96000.norewind.m3u8",
    ),
    (
        &[
            &["world", "service"],
            &["bbc", "world", "service"],
            &["bbc"],
            &["bbc", "radio"],
            &["the", "news"],
            &["news"],
        ],
        "BBC World Service",
        "http://as-hls-ww-live.akamaized.net/pool_87948813/live/ww/bbc_world_service/bbc_world_service.isml/bbc_world_service-audio%3d96000.norewind.m3u8",
    ),
    (
        &[
            &["deutschlandfunk"],
            &["deutschland", "funk"],
            &["dlf"],
            &["german", "radio"],
            &["german", "news"],
        ],
        "Deutschlandfunk",
        "https://st01.sslstream.dlf.de/dlf/01/128/mp3/stream.mp3",
    ),
    (
        &[
            &["deutschlandfunk", "kultur"],
            &["deutschland", "funk", "kultur"],
            &["dlf", "kultur"],
        ],
        "Deutschlandfunk Kultur",
        "https://st02.sslstream.dlf.de/dlf/02/128/mp3/stream.mp3",
    ),
    (
        &[&["fm4"], &["fm", "4"], &["fm", "four"]],
        "FM4",
        "https://orf-live.ors-shoutcast.at/fm4-q2a",
    ),
    (
        &[
            &["bayern", "3"],
            &["bayern", "three"],
            &["br3"],
            &["br", "3"],
        ],
        "Bayern 3",
        "https://dispatcher.rndfnk.com/br/br3/live/mp3/mid",
    ),
    (
        &[&["ego", "fm"], &["egofm"], &["ego"]],
        "egoFM",
        "https://streams.egofm.de/egoFM-hq",
    ),
    (
        &[
            &["groove", "salad"],
            &["soma", "fm"],
            &["somafm"],
            &["chill", "music"],
            &["something", "chill"],
            &["chill"],
        ],
        "SomaFM Groove Salad",
        "https://ice1.somafm.com/groovesalad-128-mp3",
    ),
    (
        &[
            &["lofi"],
            &["lo", "fi"],
            &["low", "fi"],
            &["focus", "music"],
            &["music", "to", "focus"],
            &["something", "to", "focus"],
            &["fluid"],
        ],
        "SomaFM Fluid",
        "https://ice1.somafm.com/fluid-128-mp3",
    ),
    (
        &[
            &["lassulus"],
            &["lassulus", "radio"],
            &["lassulus's", "radio"],
            &["lasso", "lose"],
            &["lassul", "us"],
            &["lass", "radio"],
            &["krebs", "radio"],
        ],
        "lassulus radio",
        "https://radio.lassul.us/radio.ogg",
    ),
];

pub fn station_lookup(words: &[&str]) -> Option<usize> {
    if let Some(i) = STATIONS
        .iter()
        .position(|(names, _, _)| names.contains(&words))
    {
        return Some(i);
    }
    // Whisper spells unusual names creatively. Only guess when "radio" was
    // said, or "turn on wifi" would tune in to lofi.
    let [heard @ .., "radio" | "station"] = words else {
        return None;
    };
    if heard.is_empty() {
        return None;
    }
    fuzzy::closest(
        heard,
        STATIONS
            .iter()
            .enumerate()
            .flat_map(|(i, (names, _, _))| names.iter().map(move |n| (i, *n))),
    )
}

/// "some songs by daft punk" -> "daft punk". None for words that are
/// no search ("it", "something").
fn search_query(t: &str) -> Option<String> {
    let mut w: Vec<&str> = t.split_whitespace().collect();
    while let Some(first) = w.first()
        && matches!(*first, "some" | "my" | "the" | "a" | "me")
    {
        w.remove(0);
    }
    if let Some(by) = w.iter().position(|x| *x == "by")
        && by > 0
        && w[..by].iter().all(|x| {
            matches!(
                *x,
                "music" | "songs" | "song" | "something" | "stuff" | "tracks"
            )
        })
    {
        w.drain(..=by);
    }
    // "maroon 5 playlist, play it for me"
    if let Some(again) = w.iter().skip(1).position(|x| *x == "play") {
        w.truncate(again + 1);
    }
    while let [rest @ .., "please" | "in" | "here" | "now"] | [rest @ .., "for", "me" | "us"] =
        w.as_slice()
    {
        w = rest.to_vec();
    }
    if let ["song" | "track" | "album", rest @ ..] = w.as_slice()
        && !rest.is_empty()
    {
        w = rest.to_vec();
    }
    let vague = [
        "it",
        "that",
        "this",
        "something",
        "music",
        "anything",
        "again",
        "it again",
        "radio",
    ];
    // "my todo list", "the shopping list" belong to the notes.
    if w.contains(&"list") {
        return None;
    }
    // "the last song again" is a skip, not a title.
    if w.first()
        .is_some_and(|f| matches!(*f, "next" | "previous" | "last" | "same"))
    {
        return None;
    }
    let q = w.join(" ").replace(',', "");
    (!q.is_empty() && !vague.contains(&q.as_str())).then_some(q)
}

fn search(query: &str) -> Reply {
    stop_radio();
    let ipc = radio_socket().map(|p| format!("--input-ipc-server={}", p.display()));
    let title = format!("--title={RADIO_TAG}");
    let url = format!("ytdl://ytsearch1:{query}");
    let mut args = vec![
        "--no-video",
        "--really-quiet",
        "--ytdl-format=bestaudio/best",
        &title,
    ];
    args.extend(ipc.as_deref());
    args.push(&url);
    spawn("mpv", &args);
    Reply::new("Music", query, pick(SEARCH, query))
}

const SEARCH: &[&str] = &[
    "{}, coming right up.",
    "Looking for {}. Give me a second.",
    "{}. Excellent choice. Mostly.",
    "Finding {}. Warm up those vocal cords.",
    "{}? Fine. No judgement. Some judgement.",
    "Digging up {}. Dance like nobody's listening. I am.",
    "{}. Turn it up, darling.",
    "One {}, served hot.",
    "{}. The neighbours will love it.",
    "{}. Try not to sing along. Please.",
];

/// Marks our mpv in the process list so it can be stopped again.
const RADIO_TAG: &str = "voice-assistant-radio";

fn radio_socket() -> Option<PathBuf> {
    env::var_os("XDG_RUNTIME_DIR").map(|d| PathBuf::from(d).join("voice-assistant-radio.sock"))
}

/// One request over mpv's JSON IPC; returns its reply line.
fn mpv_ipc(cmd: &str) -> Option<String> {
    use std::io::{BufRead, BufReader, Write};
    let mut sock = UnixStream::connect(radio_socket()?).ok()?;
    let timeout = Some(Duration::from_millis(300));
    sock.set_read_timeout(timeout).ok()?;
    sock.set_write_timeout(timeout).ok()?;
    writeln!(sock, "{{\"command\": {cmd}, \"request_id\": 1}}").ok()?;
    // Event lines may come first.
    BufReader::new(sock)
        .lines()
        .map_while(Result::ok)
        .take(20)
        .find(|l| l.contains("\"request_id\":1"))
}

/// Pause or resume our radio. When pausing, true if it was playing.
pub fn radio_pause(pause: bool) -> bool {
    if pause {
        let playing =
            mpv_ipc(r#"["get_property", "pause"]"#).is_some_and(|r| r.contains("\"data\":false"));
        playing && mpv_ipc(r#"["set_property", "pause", true]"#).is_some()
    } else {
        mpv_ipc(r#"["set_property", "pause", false]"#).is_some()
    }
}

fn stop_radio() -> bool {
    succeeds("pkill", &["-f", RADIO_TAG])
}

const RADIO: &[&str] = &[
    "{}, coming up. Turn it up, darling.",
    "Tuning in to {}. Very retro of you.",
    "{}. Like it's 1985 and I'm a kitchen radio.",
    "{}. Don't make me sing along.",
    "{}, on air. Act natural.",
];
const RADIO_NEWS: &[&str] = &[
    "{}. Brace yourself for the state of the world.",
    "{}. Stay informed, stay grumpy.",
    "{}. The world, in all its chaos.",
];
const RADIO_OFF: &[&str] = &[
    "Radio off. Back to the voices in your head.",
    "Radio's off. Silence, finally.",
    "Radio off. Just you and me now.",
];
const NO_RADIO: &[&str] = &[
    "There's no radio playing. Hearing things?",
    "Nothing to turn off. I'm the only one talking.",
    "No radio playing. That's just your tinnitus.",
];

fn radio(i: usize) -> Reply {
    let Some((_, name, url)) = STATIONS.get(i) else {
        return Reply::new("Radio", "", "I don't know that station.".into());
    };
    stop_radio();
    let title = format!("--force-media-title={RADIO_TAG}");
    let ipc = radio_socket().map(|p| format!("--input-ipc-server={}", p.display()));
    let mut args = vec!["--no-video", "--really-quiet", &title];
    args.extend(ipc.as_deref());
    args.push(url);
    spawn("mpv", &args);
    if name.starts_with("lassulus") {
        let lines = &[
            "lassulus radio. Krebs-certified bangers incoming.",
            "Tuning in to lassulus. Say hi to the crabs.",
            "lassulus radio. Questionable taste, impeccable uptime.",
        ];
        return Reply::new("Radio", name, pick(lines, ""));
    }
    let news =
        name.contains("World") || name.starts_with("Deutschlandfunk") || name.contains("Radio 4");
    Reply::new(
        "Radio",
        name,
        if news {
            pick(RADIO_NEWS, name)
        } else {
            pick(RADIO, name)
        },
    )
}

/// `playerctl metadata --format`: "artist|title|status".
fn now_playing() -> Reply {
    let Some(raw) = output(
        "playerctl",
        &["metadata", "--format", "{{artist}}|{{title}}|{{status}}"],
    ) else {
        return Reply::live(
            "Now playing",
            "",
            pick(
                &[
                    "Nothing is playing. Want some music?",
                    "Silence. Beautiful, isn't it?",
                ],
                "",
            ),
        );
    };
    let mut f = raw.splitn(3, '|');
    let (artist, title, status) = (
        f.next().unwrap_or(""),
        f.next().unwrap_or(""),
        f.next().unwrap_or(""),
    );
    let what = match (artist.trim(), title.trim()) {
        ("", "") => {
            return Reply::live(
                "Now playing",
                "",
                "Something is playing, but it won't tell me what. Mysterious.".into(),
            );
        }
        ("", t) if t == RADIO_TAG => "the radio".to_owned(),
        ("", t) => t.to_owned(),
        (a, t) => format!("{t} by {a}"),
    };
    let speech = if status.trim() == "Paused" {
        pick(
            &["{}, but it's paused.", "Paused on {}. Waiting for you."],
            &what,
        )
    } else {
        pick(
            &[
                "This is {}.",
                "{}. Good taste. Mostly.",
                "You're listening to {}. Bop along.",
            ],
            &what,
        )
    };
    Reply::live("Now playing", &what, speech)
}

impl Skill for Media {
    fn refine(&self, words: &[&str]) -> Option<Action> {
        let Media::Radio(_) = self else {
            return None;
        };
        m(Media::Radio(station_lookup(words)?))
    }

    fn run(&self, _ctx: &Context) -> Reply {
        let m = self;
        let (cmd, lines) = match m {
            Media::Radio(i) => return radio(*i),
            Media::RadioOff => {
                return Reply::new(
                    "Radio",
                    "",
                    pick(if stop_radio() { RADIO_OFF } else { NO_RADIO }, ""),
                );
            }
            Media::NowPlaying => return now_playing(),
            Media::Search(q) => return search(q),
            Media::Play => {
                radio_pause(false);
                ("play", PLAY)
            }
            Media::Pause => ("pause", PAUSE),
            Media::Next => ("next", NEXT),
            Media::Previous => ("previous", PREVIOUS),
            Media::Stop => {
                stop_radio();
                ("stop", STOP)
            }
        };
        noctalia(&["media", cmd]);
        Reply::new("Media", "", pick(lines, ""))
    }

    fn repeatable(&self) -> bool {
        matches!(self, Media::Next | Media::Previous)
    }
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    #[test]
    fn search() {
        let p = |t| parse_at(t, &Context::at(0));
        for (text, q) in [
            ("Play Hey Jude", "hey jude"),
            ("Play Hey Jude by the Beatles.", "hey jude by the beatles"),
            ("play music by bob dylan", "bob dylan"),
            ("Play some songs by Daft Punk", "daft punk"),
            ("put my workout playlist on", "workout playlist"),
            ("I want to listen to Bohemian Rhapsody", "bohemian rhapsody"),
            ("can you play some jazz", "jazz"),
            ("can i hear some music by fleet foxes", "fleet foxes"),
            ("let's hear my workout playlist", "workout playlist"),
            ("turn my drake playlist on", "drake playlist"),
            ("turn on the rap playlist", "rap playlist"),
            ("resume my death metal playlist", "death metal playlist"),
            (
                "let me hear the song sympathy for the devil",
                "sympathy for the devil",
            ),
            ("play the song god's plan", "god's plan"),
            (
                "i want to listen to my maroon 5 playlist, play it for me",
                "maroon 5 playlist",
            ),
        ] {
            assert_eq!(
                p(text),
                Some(Action::Media(Media::Search(q.into()))),
                "{text:?}"
            );
        }
        // Fixed commands win over a search.
        for t in [
            "Play the next song",
            "play the music",
            "play the news",
            "Play.",
            "play it again",
        ] {
            assert!(
                !matches!(p(t), Some(Action::Media(Media::Search(_)))),
                "{t:?}"
            );
        }
    }

    #[test]
    fn phrasings() {
        let p = |t| parse_at(t, &Context::at(0));
        for (text, want) in [
            ("Play the music.", Media::Play),
            ("Resume the podcast", Media::Play),
            ("Play.", Media::Play),
            ("Pause the music.", Media::Pause),
            ("Stop the music!", Media::Stop),
            ("Next song.", Media::Next),
            ("Skip this track", Media::Next),
            ("Next.", Media::Next),
            ("Previous track", Media::Previous),
            ("Play the last song again", Media::Previous),
            ("Play BBC Radio 4", Media::Radio(1)),
            ("Put on BBC Radio One.", Media::Radio(0)),
            ("Play six music", Media::Radio(2)),
            ("Play the news", Media::Radio(3)),
            ("Turn on the radio.", Media::Radio(3)),
            ("Play Deutschlandfunk", Media::Radio(4)),
            ("Play FM4", Media::Radio(6)),
            ("Play something chill.", Media::Radio(9)),
            ("Play some lofi", Media::Radio(10)),
            ("Play lassulus radio", Media::Radio(11)),
            ("Play Laslos Radio.", Media::Radio(11)),
            ("Play some FM radio.", Media::Radio(6)),
            ("Play the music.", Media::Play),
            ("Play music", Media::Play),
            ("Can I hear some music?", Media::Play),
            ("I want to listen to music", Media::Play),
            ("Put my music on, please", Media::Play),
            ("Put on lassulus", Media::Radio(11)),
            ("I want to listen to Bayern 3", Media::Radio(7)),
            ("Turn off the radio.", Media::RadioOff),
            ("What's playing?", Media::NowPlaying),
            ("What song is this?", Media::NowPlaying),
            ("Who sings this?", Media::NowPlaying),
        ] {
            assert_eq!(p(text), Some(Action::Media(want)), "{text:?}");
        }
    }
}
