//! Machines, network, background jobs and resources.

use crate::http::{Ip, get};

const IP_URL: &str = "https://ip.thalheim.io";
use super::json::{key, text};
use super::succeeds;
use super::{Action, Context, Intent, Module, Reply, Skill, choose, intent, output, pick, spawn};
use crate::grammar::Value;
use std::fs;
use std::num::NonZero;
use std::thread;
use tinyjson::JsonValue;

#[derive(Debug, Clone, PartialEq)]
pub enum Ops {
    /// Index into `HOSTS`.
    HostUp(usize),
    Internet,
    Ip,
    Jobs,
    Disk,
    Memory,
    Cpu,
    FindPhone,
    /// Index into `BT_DEVICES`.
    Bluetooth {
        device: usize,
        connect: bool,
    },
}

/// Spoken names (after normalize) and the host to ping.
pub const HOSTS: &[(&[&[&str]], &str)] = &[
    (&[&["eve"], &["the", "server"]], "eve.r"),
    (&[&["eva"], &["ava"]], "eva.r"),
    (&[&["blob", "64"], &["blob"]], "blob64.r"),
    (&[&["evo"]], "evo.r"),
    (
        &[
            &["turing", "machine"],
            &["turingmachine"],
            &["the", "desktop"],
        ],
        "turingmachine.r",
    ),
    (&[&["bernie"], &["the", "laptop"]], "bernie.r"),
];

/// Spoken names and a substring of the paired device's name.
pub const BT_DEVICES: &[(&[&[&str]], &str, &str)] = &[
    (
        &[
            &["headphones"],
            &["headphone"],
            &["headset"],
            &["audeze"],
            &["maxwell"],
            &["big", "headphones"],
        ],
        "Audeze",
        "headphones",
    ),
    (
        &[
            &["earbuds"],
            &["earphones"],
            &["beats"],
            &["in", "ears"],
            &["beats", "flex"],
        ],
        "Beats",
        "earbuds",
    ),
    (&[&["mouse"], &["mx", "master"]], "MX Master", "mouse"),
];

pub fn bt_lookup(words: &[&str]) -> Option<usize> {
    BT_DEVICES
        .iter()
        .position(|(names, _, _)| names.contains(&words))
}

/// "Device E0:49:ED:04:B7:B4 Audeze Maxwell BT" -> the MAC of the first match.
fn bt_mac(devices: &str, needle: &str) -> Option<String> {
    devices.lines().find_map(|l| {
        let rest = l.trim().strip_prefix("Device ")?;
        let (mac, name) = rest.split_once(' ')?;
        name.to_lowercase()
            .contains(&needle.to_lowercase())
            .then(|| mac.to_owned())
    })
}

fn bluetooth(device: usize, connect: bool) -> Reply {
    let Some((_, needle, what)) = BT_DEVICES.get(device) else {
        return Reply::live(
            "Bluetooth",
            "",
            pick(
                &[
                    "Which device? I only know the ones you paired.",
                    "Connect what? Be specific, I'm not a mind reader.",
                ],
                "",
            ),
        );
    };
    let devices = output("bluetoothctl", &["devices", "Paired"]).unwrap_or_default();
    let Some(mac) = bt_mac(&devices, needle) else {
        return Reply::live(
            "Bluetooth",
            "",
            format!(
                "I can't find your {what} among the paired devices. Pair first, I'm not a magician."
            ),
        );
    };
    // Connecting blocks for seconds; the reply should not wait for it.
    spawn(
        "sh",
        &[
            "-c",
            if connect {
                "bluetoothctl power on >/dev/null; exec bluetoothctl connect \"$0\""
            } else {
                "exec bluetoothctl disconnect \"$0\""
            },
            &mac,
        ],
    );
    let speech = if connect {
        pick(
            &[
                "Connecting your {}. Give it a second.",
                "{}, coming right up.",
                "Calling your {}. Hope they pick up.",
                "Pairing up with your {}. Get a room, you two.",
            ],
            what,
        )
    } else {
        pick(
            &[
                "Disconnecting your {}.",
                "Bye bye, {}.",
                "{} dumped. It's not you, it's Bluetooth.",
                "{} cut loose.",
            ],
            what,
        )
    };
    Reply::live(
        "Bluetooth",
        &format!(
            "{} {needle}",
            if connect { "connect" } else { "disconnect" }
        ),
        speech,
    )
}

pub fn lookup(words: &[&str]) -> Option<usize> {
    HOSTS.iter().position(|(names, _)| names.contains(&words))
}

fn o(x: Ops) -> Option<Action> {
    Some(Action::Ops(x))
}

pub const MODULE: Module = Module {
    intents,
    slots: &[
        ("bt", |w, _| bt_lookup(w).map(Value::Index)),
        ("host", |w, _| lookup(w).map(Value::Index)),
    ],
    vocab: &[
        "eva",
        "blob64",
        "evo",
        "turingmachine",
        "Audeze",
        "MX Master",
        "pueue",
    ],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            Some("Is eve up?"),
            &[
                "!(is|are) {host} (up|down|online|offline|alive|reachable|running)",
                "!can you (reach|ping) {host}",
                "!ping {host}",
                "!(check|how is|how's) {host}",
            ],
            |s, _| match s.get("host")? {
                Value::Index(i) => o(Ops::HostUp(*i)),
                _ => None,
            },
        ),
        intent(
            None,
            &[
                "(is|are) [the|my] (internet|network|wifi|connection) (down|up|working|broken|ok|slow)",
                "(am|are) (i|we) (online|offline|connected)",
                "do i have internet",
                "!ping [the] internet",
            ],
            |_, _| o(Ops::Internet),
        ),
        intent(
            None,
            &[
                "what's my ip [address]",
                "what is my ip [address]",
                "!ip address",
            ],
            |_, _| o(Ops::Ip),
        ),
        intent(
            None,
            &[
                "(is|are) [my] (build|builds|job|jobs|task|tasks) (done|finished|ready|running|still running)",
                "(how's|how is|how are) [my] (build|builds|job|jobs|task|tasks) [doing|going]",
                "(did|has) [my] (build|job|task) (finish|finished|fail|failed|succeed|succeeded|pass|passed)",
                "what's (running|in the queue)",
                "!(pueue|queue) [status]",
            ],
            |_, _| o(Ops::Jobs),
        ),
        intent(
            None,
            &[
                "how much (disk|disc|storage) [space] [is] [left|free|do i have]",
                "(is|am) [the|my] (disk|disc) [running] (full|out of space)",
                "!(disk|disc) space",
            ],
            |_, _| o(Ops::Disk),
        ),
        intent(
            None,
            &[
                "how much (memory|ram) [is] [left|free|used|do i have]",
                "!(memory|ram) [usage]",
            ],
            |_, _| o(Ops::Memory),
        ),
        intent(
            None,
            &[
                "why is [the|my] (computer|laptop|fan) [so] (slow|loud|hot)",
                "what's (using|eating|hogging) [all] [the|my] (cpu|processor|memory)",
                "how busy is [the|my] (computer|cpu|machine)",
                "!(cpu|load) [usage]",
            ],
            |_, _| o(Ops::Cpu),
        ),
        intent(
            Some("Find my phone."),
            &[
                "!(find|ring|locate|where is|where's) my (phone|android|mobile|cell phone|handy)",
                "!(i|i've) (lost|can't find|cannot find) my (phone|mobile|handy)",
                "!(make|let) my phone (ring|beep)",
            ],
            |_, _| o(Ops::FindPhone),
        ),
        intent(
            Some("Connect my headphones."),
            &[
                "!(connect|pair|hook up) [my|the] {bt} [please]",
                "!(connect|switch) [the] (audio|sound) to [my|the] {bt}",
                "!(use|switch to) [my|the] {bt}",
            ],
            |s, _| match s.get("bt")? {
                Value::Index(i) => o(Ops::Bluetooth {
                    device: *i,
                    connect: true,
                }),
                _ => None,
            },
        ),
        intent(
            None,
            &["!(disconnect|unpair|unhook) [my|the] {bt}"],
            |s, _| match s.get("bt")? {
                Value::Index(i) => o(Ops::Bluetooth {
                    device: *i,
                    connect: false,
                }),
                _ => None,
            },
        ),
    ]
}

fn find_phone() -> Reply {
    let ids = output("kdeconnect-cli", &["--list-available", "--id-only"]).unwrap_or_default();
    let ids: Vec<&str> = ids
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .collect();
    if ids.is_empty() {
        let speech = pick(
            &[
                "Your phone isn't connected. Check the couch cushions.",
                "I can't reach your phone. It's hiding from you. Smart phone.",
                "No phone in range. Did you leave it at the bar again?",
                "No phone. Check the fridge, that's where it was last time.",
            ],
            "",
        );
        return Reply::live("Find phone", "no device connected", speech);
    }
    for id in &ids {
        spawn("kdeconnect-cli", &["--device", id, "--ring"]);
    }
    let speech = pick(
        &[
            "Ringing it now. Follow the noise.",
            "Your phone is screaming somewhere. Go find it.",
            "Ring ring! Marco... Polo.",
            "It's ringing. Follow the sound of your shame.",
        ],
        "",
    );
    Reply::live("Find phone", "ringing", speech)
}

fn ping(host: &str) -> bool {
    succeeds("ping", &["-c", "1", "-W", "2", host])
}

impl Skill for Ops {
    fn refine(&self, words: &[&str]) -> Option<Action> {
        match self {
            Ops::HostUp(_) => Some(Action::Ops(Ops::HostUp(lookup(words)?))),
            Ops::Bluetooth { connect, .. } => Some(Action::Ops(Ops::Bluetooth {
                device: bt_lookup(words)?,
                connect: *connect,
            })),
            _ => None,
        }
    }

    fn run(&self, _ctx: &Context) -> Reply {
        let x = self;
        match x {
            Ops::HostUp(i) => host_up(*i),
            Ops::Internet => internet(),
            Ops::Ip => ip(),
            Ops::Jobs => jobs(),
            Ops::Disk => disk(),
            Ops::Memory => memory(),
            Ops::Cpu => cpu(),
            Ops::FindPhone => find_phone(),
            Ops::Bluetooth { device, connect } => bluetooth(*device, *connect),
        }
    }
    fn filler(&self) -> Option<&'static [&'static str]> {
        matches!(self, Ops::HostUp(_) | Ops::Internet | Ops::Ip).then_some(&[
            "Checking.",
            "One moment, pinging.",
            "Poking the network with a stick.",
            "Knock knock. Let's see who's home.",
        ])
    }
}

fn host_up(i: usize) -> Reply {
    let Some((names, host)) = HOSTS.get(i) else {
        return Reply::live(
            "Hosts",
            "",
            pick(
                &[
                    "Never heard of that machine.",
                    "Never heard of that machine. Is it imaginary, like your gym routine?",
                ],
                "",
            ),
        );
    };
    let name = names.first().map(|n| n.join(" ")).unwrap_or_default();
    let name = capitalize(&name);
    if ping(host) {
        let speech = pick(
            &[
                "{} is up and purring.",
                "{} answers. Alive and well.",
                "Yep, {} is fine. Stop worrying.",
                "{} is up. Unlike you before coffee.",
            ],
            &name,
        );
        Reply::live("Hosts", &format!("{name} is up"), speech)
    } else {
        let speech = pick(
            &[
                "{} is not answering. Uh oh.",
                "{} is down. Or ignoring us. Rude either way.",
                "No reply from {}. Time to panic? A little?",
                "{} is ghosting us. Rude.",
            ],
            &name,
        );
        Reply::live("Hosts", &format!("{name} is down"), speech)
    }
}

fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    c.next()
        .map(|f| f.to_uppercase().chain(c).collect())
        .unwrap_or_default()
}

fn internet() -> Reply {
    if ping("1.1.1.1") {
        let speech = if ping("eve.thalheim.io") {
            pick(
                &[
                    "The internet works. Whatever is broken, it's you.",
                    "Online. Cat videos are a go.",
                    "Online. Go on, doomscroll.",
                ],
                "",
            )
        } else {
            "I can reach the internet, but not names. Smells like DNS. It's always DNS.".to_owned()
        };
        Reply::live("Network", "online", speech)
    } else {
        Reply::live(
            "Network",
            "offline",
            pick(
                &[
                    "We're offline. Go outside, I guess.",
                    "No internet. Have you tried turning it off and on again?",
                    "Offline. Read a book. Remember those?",
                ],
                "",
            ),
        )
    }
}

fn ip() -> Reply {
    // "1.1.1.1 via 192.168.1.1 dev wlan0 src 192.168.1.23 uid 1000"
    let local = output("ip", &["-4", "route", "get", "1.1.1.1"]).and_then(|r| {
        r.split_whitespace()
            .skip_while(|w| *w != "src")
            .nth(1)
            .map(str::to_owned)
    });
    let public = get(IP_URL).timeout(4).ip(Ip::V4).text();
    let v6 = get(IP_URL).timeout(4).ip(Ip::V6).text();
    let body = [
        local.as_deref().map(|l| format!("local {l}")),
        public.as_deref().map(|p| format!("public {p}")),
        v6.as_deref().map(|p| format!("IPv6 {p}")),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n");
    let speech = match (&local, &public) {
        (None, None) => "No IP at all. We're floating in the void.".to_owned(),
        (Some(l), None) => format!("Your local address is {l}. The outside world is unreachable."),
        (_, Some(p)) => {
            let six = if v6.is_some() {
                " IPv6 too, it's in the notification."
            } else {
                " No IPv6. How retro."
            };
            format!("Your public IP is {p}.{six} Don't give it to strangers.")
        }
    };
    Reply::live("IP address", &body, speech)
}

#[derive(Debug, Default, PartialEq)]
struct Queue {
    running: usize,
    queued: usize,
    /// Most recently finished: command and whether it succeeded.
    last: Option<(String, bool)>,
}

/// `pueue status --json`: a status is `{"Running": {..}}`, `{"Queued": {..}}`
/// or `{"Done": {"end": .., "result": "Success" | {"Failed": 1} | ..}}`.
fn parse_queue(json: &str) -> Option<Queue> {
    let v: JsonValue = json.parse().ok()?;
    let JsonValue::Object(tasks) = key(&v, "tasks")? else {
        return None;
    };
    let mut q = Queue::default();
    let mut last_end = String::new();
    for t in tasks.values() {
        let Some(JsonValue::Object(status)) = key(t, "status") else {
            continue;
        };
        if status.contains_key("Running") {
            q.running += 1;
        } else if status.contains_key("Queued") || status.contains_key("Stashed") {
            q.queued += 1;
        } else if let Some(done) = status.get("Done") {
            let end = key(done, "end").and_then(text).unwrap_or_default();
            if end > last_end.as_str() {
                last_end = end.to_owned();
                let ok = key(done, "result").and_then(text) == Some("Success");
                let cmd = key(t, "command").and_then(text).unwrap_or_default();
                q.last = Some((cmd.to_owned(), ok));
            }
        }
    }
    Some(q)
}

/// "nix build --keep-going .#foo" -> "nix build".
fn short_command(cmd: &str) -> String {
    cmd.split_whitespace()
        .take_while(|w| !w.starts_with('-') && !w.contains(['/', '#', '.']))
        .take(2)
        .collect::<Vec<_>>()
        .join(" ")
}

fn jobs() -> Reply {
    let Some(q) = output("pueue", &["status", "--json"])
        .as_deref()
        .and_then(parse_queue)
    else {
        return Reply::live(
            "Jobs",
            "",
            pick(
                &[
                    "I can't reach pueue. Is the daemon sleeping too?",
                    "Pueue isn't answering. Lazy little daemon.",
                ],
                "",
            ),
        );
    };
    let last = q.last.as_ref().map(|(cmd, ok)| {
        let what = short_command(cmd);
        if *ok {
            format!("The last one, {what}, worked.")
        } else {
            format!("The last one, {what}, failed. Oops.")
        }
    });
    let body = format!("{} running, {} queued", q.running, q.queued);
    let speech = match (q.running, q.queued) {
        (0, 0) => format!(
            "Nothing running. {}",
            last.unwrap_or_else(|| "Queue is empty. Lazy day?".into())
        ),
        (1, 0) => format!(
            "One job is still running. Patience, darling. {}",
            last.unwrap_or_default()
        ),
        (r, 0) => format!(
            "{r} jobs running. Your CPU is sweating. {}",
            last.unwrap_or_default()
        ),
        (r, n) => format!("{r} running and {n} waiting in line. Get a coffee."),
    };
    Reply::live("Jobs", &body, speech.trim().to_owned())
}

fn disk() -> Reply {
    // "Avail Use%\n  123G  45%"
    let raw = output("df", &["--output=avail,pcent", "-BG", "/home"]).unwrap_or_default();
    let mut f = raw.lines().nth(1).unwrap_or_default().split_whitespace();
    let (Some(avail), Some(pct)) = (f.next(), f.next()) else {
        return Reply::live(
            "Disk",
            "",
            pick(
                &[
                    "I can't read the disk. Spooky.",
                    "The disk won't talk to me. Probably full of your downloads.",
                ],
                "",
            ),
        );
    };
    let avail: u64 = avail.trim_end_matches('G').parse().unwrap_or(0);
    let pct: u64 = pct.trim_end_matches('%').parse().unwrap_or(0);
    let what = format!("{avail} gigabytes free, {pct} percent used");
    let speech = choose(
        &[
            (
                pct >= 95,
                &[
                    "{}. We're full! Delete something. Maybe old nix generations?",
                    "{}. Run garbage collection, I'm begging you.",
                ][..],
            ),
            (
                pct >= 80,
                &[
                    "{}. Getting tight. Nix store again?",
                    "{}. Not critical, but I'm judging.",
                ][..],
            ),
        ],
        &["{}. Plenty of room.", "{}. Hoard away."],
        &what,
    );
    Reply::live("Disk", &what, speech)
}

fn meminfo(raw: &str, field: &str) -> Option<u64> {
    raw.lines().find_map(|l| {
        l.strip_prefix(field)?
            .strip_prefix(':')?
            .split_whitespace()
            .next()?
            .parse()
            .ok()
    })
}

fn memory() -> Reply {
    let raw = fs::read_to_string("/proc/meminfo").unwrap_or_default();
    let (Some(total), Some(avail)) = (meminfo(&raw, "MemTotal"), meminfo(&raw, "MemAvailable"))
    else {
        return Reply::live(
            "Memory",
            "",
            pick(
                &[
                    "I forgot how much memory we have. Ironic.",
                    "Memory? What memory? Exactly.",
                ],
                "",
            ),
        );
    };
    let used_pct = 100 - avail * 100 / total.max(1);
    let what = format!(
        "{} gigabytes free, {used_pct} percent used",
        avail / 1024 / 1024
    );
    let speech = choose(
        &[(
            used_pct >= 90,
            &[
                "{}. Close some browser tabs, hoarder.",
                "{}. I can barely think in here.",
            ][..],
        )],
        &["{}.", "{}. Room for more tabs. Unfortunately."],
        &what,
    );
    Reply::live("Memory", &what, speech)
}

fn cpu() -> Reply {
    let load = fs::read_to_string("/proc/loadavg")
        .ok()
        .and_then(|l| l.split_whitespace().next()?.parse::<f64>().ok())
        .unwrap_or(0.0);
    let cores = thread::available_parallelism().map_or(1, NonZero::get);
    // "firefox  123.4"
    let top = output("ps", &["-eo", "comm,%cpu", "--sort=-%cpu", "--no-headers"])
        .and_then(|o| o.lines().next().map(str::to_owned))
        .and_then(|l| {
            let (name, pct) = l.trim().rsplit_once(char::is_whitespace)?;
            Some((name.trim().to_owned(), pct.parse::<f64>().ok()?))
        });
    let load_pct = load * 100.0 / f64::from(u32::try_from(cores).unwrap_or(1));
    let what = format!("load {load:.1} on {cores} cores");
    let speech = match top {
        Some((name, pct)) if pct >= 50.0 => {
            format!("{name} is hogging the CPU at {pct:.0} percent. Want me to judge it?")
        }
        _ if load_pct >= 80.0 => format!("Busy! {what}. Something is compiling, I bet."),
        _ => pick(
            &[
                "Chill. {}. I'm barely awake.",
                "{}. The CPU is napping. Lucky thing.",
                "{}. Nothing to worry about.",
            ],
            &what,
        ),
    };
    Reply::live("CPU", &what, speech)
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    #[test]
    fn phrasings() {
        let p = |t| parse_at(t, &Context::at(0));
        for (text, want) in [
            ("Is eve up?", Ops::HostUp(0)),
            ("Is the server down?", Ops::HostUp(0)),
            ("Can you ping blob 64?", Ops::HostUp(2)),
            ("Is turingmachine online?", Ops::HostUp(4)),
            ("Is the internet down?", Ops::Internet),
            ("Find my phone.", Ops::FindPhone),
            (
                "Connect my headphones.",
                Ops::Bluetooth {
                    device: 0,
                    connect: true,
                },
            ),
            (
                "Switch the audio to the earbuds",
                Ops::Bluetooth {
                    device: 1,
                    connect: true,
                },
            ),
            (
                "Disconnect the mouse",
                Ops::Bluetooth {
                    device: 2,
                    connect: false,
                },
            ),
            ("Where's my phone?", Ops::FindPhone),
            ("I can't find my phone!", Ops::FindPhone),
            ("Am I online?", Ops::Internet),
            ("What's my IP address?", Ops::Ip),
            ("Is my build done?", Ops::Jobs),
            ("How are my jobs doing?", Ops::Jobs),
            ("Did my build fail?", Ops::Jobs),
            ("How much disk space is left?", Ops::Disk),
            ("How much memory is free?", Ops::Memory),
            ("Why is my computer so slow?", Ops::Cpu),
            ("What's eating my CPU?", Ops::Cpu),
        ] {
            assert_eq!(p(text), Some(Action::Ops(want)), "{text:?}");
        }
    }

    #[test]
    fn queue_parsing() {
        let json = r#"{"tasks":{
            "1":{"command":"nix build .#a","status":{"Done":{"end":"2026-10-17T15:00:00","result":"Success"}}},
            "2":{"command":"merge-when-green --message x","status":{"Done":{"end":"2026-10-18T15:00:00","result":{"Failed":1}}}},
            "3":{"command":"cargo test","status":{"Running":{"start":"x"}}},
            "4":{"command":"sleep 1","status":{"Queued":{"enqueued_at":"x"}}}
        },"groups":{}}"#;
        let q = parse_queue(json).expect("parses");
        assert_eq!((q.running, q.queued), (1, 1));
        assert_eq!(q.last, Some(("merge-when-green --message x".into(), false)));
        assert_eq!(
            short_command("merge-when-green --message x"),
            "merge-when-green"
        );
        assert_eq!(short_command("nix build --keep-going .#foo"), "nix build");
    }

    #[test]
    fn bluetooth_devices() {
        let raw =
            "Device E0:49:ED:04:B7:B4 Audeze Maxwell BT\nDevice 0C:53:B7:B5:B2:49 Beats Flex\n";
        assert_eq!(bt_mac(raw, "beats"), Some("0C:53:B7:B5:B2:49".into()));
        assert_eq!(bt_mac(raw, "Audeze"), Some("E0:49:ED:04:B7:B4".into()));
        assert_eq!(bt_mac(raw, "MX Master"), None);
    }

    #[test]
    fn meminfo_parsing() {
        let raw = "MemTotal:       32000000 kB\nMemFree:  100 kB\nMemAvailable:   16000000 kB\n";
        assert_eq!(meminfo(raw, "MemTotal"), Some(32_000_000));
        assert_eq!(meminfo(raw, "MemAvailable"), Some(16_000_000));
        assert_eq!(meminfo(raw, "Mem"), None);
    }
}
