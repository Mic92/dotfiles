//! tmux-like automatic tab names for herdr.
//!
//! `herdr-autoname event` renames every tab after its foreground program.
//! `preexec <cmdline>` / `precmd <shell>` (called by the zsh hook) rename only
//! the current tab. A tab renamed by the user is left alone: a state file
//! remembers the last name we set per tab, and a label that is neither that
//! name nor a herdr placeholder (a bare number) opts the tab out.

use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use std::process::Command;
use tinyjson::JsonValue;

const MAX_NAME_LEN: usize = 20;

type State = BTreeMap<String, String>;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args.first().map(String::as_str) {
        Some("preexec") => rename_current_tab(program_from_cmdline(args.get(1).map_or("", |s| s))),
        Some("precmd") => rename_current_tab(args.get(1).cloned()),
        _ => reconcile(),
    }
}

/// First word of a command line, reduced to its basename.
fn program_from_cmdline(cmdline: &str) -> Option<String> {
    let word = cmdline.split_whitespace().next()?;
    let name = word.rsplit('/').next()?.trim_start_matches('-');
    (!name.is_empty()).then(|| name.chars().take(MAX_NAME_LEN).collect())
}

/// A tab may be auto-named while its label is a herdr placeholder (empty or a
/// bare number) or the name we set last time.
fn eligible(label: &str, last_set: Option<&String>) -> bool {
    label.chars().all(|c| c.is_ascii_digit()) || last_set.is_some_and(|n| n == label)
}

// ---- herdr CLI ----

fn herdr(args: &[&str]) -> Option<JsonValue> {
    let out = Command::new("herdr").args(args).output().ok()?;
    if !out.status.success() {
        return None;
    }
    String::from_utf8(out.stdout).ok()?.parse().ok()
}

fn at<'a>(v: &'a JsonValue, path: &[&str]) -> Option<&'a JsonValue> {
    let mut v = v;
    for key in path {
        v = v.get::<HashMap<String, JsonValue>>()?.get(*key)?;
    }
    Some(v)
}

fn str_at<'a>(v: &'a JsonValue, path: &[&str]) -> Option<&'a str> {
    at(v, path)?.get::<String>().map(String::as_str)
}

fn is_true(v: &JsonValue, key: &str) -> bool {
    at(v, &[key]).and_then(JsonValue::get::<bool>) == Some(&true)
}

fn rename_tab(tab_id: &str, name: &str) {
    herdr(&["tab", "rename", tab_id, name]);
}

/// Foreground program of a pane: the process-group leader's argv0 basename.
fn pane_program(pane_id: &str) -> Option<String> {
    let v = herdr(&["pane", "process-info", "--pane", pane_id])?;
    let info = at(&v, &["result", "process_info"])?;
    let leader: &f64 = at(info, &["foreground_process_group_id"])?.get()?;
    let procs: &Vec<JsonValue> = at(info, &["foreground_processes"])?.get()?;
    let proc = procs
        .iter()
        .find(|p| at(p, &["pid"]).and_then(JsonValue::get::<f64>) == Some(leader))?;
    let argv0 = str_at(proc, &["argv0"])
        .or_else(|| {
            at(proc, &["argv"])?
                .get::<Vec<JsonValue>>()?
                .first()?
                .get::<String>()
                .map(String::as_str)
        })
        .or_else(|| str_at(proc, &["name"]))?;
    program_from_cmdline(argv0)
}

// ---- state file: one "tab_id\tname" per line ----

fn state_path() -> PathBuf {
    std::env::var_os("XDG_STATE_HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            PathBuf::from(std::env::var_os("HOME").unwrap_or_default()).join(".local/state")
        })
        .join("herdr-autoname/tabs")
}

fn load_state() -> State {
    std::fs::read_to_string(state_path())
        .unwrap_or_default()
        .lines()
        .filter_map(|l| l.split_once('\t'))
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect()
}

fn save_state(state: &State) {
    let path = state_path();
    if let Some(dir) = path.parent() {
        let _ = std::fs::create_dir_all(dir);
    }
    let lines: String = state.iter().map(|(k, v)| format!("{k}\t{v}\n")).collect();
    let _ = std::fs::write(path, lines);
}

// ---- modes ----

/// Fast path for the zsh hook: rename only the tab this shell runs in.
fn rename_current_tab(name: Option<String>) {
    let (Some(name), Ok(tab_id)) = (name, std::env::var("HERDR_TAB_ID")) else {
        return;
    };
    let Some(tab) = herdr(&["tab", "get", &tab_id]) else {
        return;
    };
    let label = str_at(&tab, &["result", "tab", "label"]).unwrap_or("");
    let mut state = load_state();
    if !eligible(label, state.get(&tab_id)) {
        return;
    }
    if label != name {
        rename_tab(&tab_id, &name);
    }
    state.insert(tab_id, name);
    save_state(&state);
}

/// Rename every tab after its active pane's foreground program.
fn reconcile() {
    let (Some(tabs), Some(panes)) = (herdr(&["tab", "list"]), herdr(&["pane", "list"])) else {
        return;
    };
    let empty = Vec::new();
    let tabs: &Vec<JsonValue> = at(&tabs, &["result", "tabs"])
        .and_then(JsonValue::get)
        .unwrap_or(&empty);
    let panes: &Vec<JsonValue> = at(&panes, &["result", "panes"])
        .and_then(JsonValue::get)
        .unwrap_or(&empty);

    let state = load_state();
    let mut seen = State::new();
    for tab in tabs {
        let Some(tab_id) = str_at(tab, &["tab_id"]) else {
            continue;
        };
        let label = str_at(tab, &["label"]).unwrap_or("");
        if let Some(last) = state.get(tab_id) {
            seen.insert(tab_id.to_string(), last.clone());
        }
        if !eligible(label, state.get(tab_id)) {
            continue;
        }
        let focused = is_true(tab, "focused");
        let Some(name) = active_pane(panes, tab_id, focused).and_then(pane_program) else {
            continue;
        };
        if label != name {
            rename_tab(tab_id, &name);
        }
        seen.insert(tab_id.to_string(), name);
    }
    if seen != state {
        save_state(&seen);
    }
}

/// The pane whose program names a tab: its only pane, or the globally focused
/// pane when the tab is focused. Unfocused multi-pane tabs are left as-is.
fn active_pane<'a>(panes: &'a [JsonValue], tab_id: &str, tab_focused: bool) -> Option<&'a str> {
    let in_tab: Vec<&JsonValue> = panes
        .iter()
        .filter(|p| str_at(p, &["tab_id"]) == Some(tab_id))
        .collect();
    let pane = match in_tab.as_slice() {
        [only] => only,
        _ if tab_focused => *in_tab.iter().find(|p| is_true(p, "focused"))?,
        _ => return None,
    };
    str_at(pane, &["pane_id"])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn program_from_cmdline_takes_basename_of_first_word() {
        assert_eq!(
            program_from_cmdline("nvim src/main.rs").as_deref(),
            Some("nvim")
        );
        assert_eq!(
            program_from_cmdline("/usr/bin/git status").as_deref(),
            Some("git")
        );
        assert_eq!(program_from_cmdline("-zsh").as_deref(), Some("zsh"));
        assert_eq!(program_from_cmdline("  "), None);
    }

    #[test]
    fn user_renamed_tabs_are_not_eligible() {
        let ours = "nvim".to_string();
        assert!(eligible("2", None)); // herdr placeholder
        assert!(eligible("nvim", Some(&ours))); // our own name
        assert!(!eligible("my tab", Some(&ours))); // user rename
        assert!(!eligible("my tab", None));
    }

    #[test]
    fn active_pane_prefers_single_pane_then_focused() {
        let panes: Vec<JsonValue> = [
            r#"{"pane_id": "p1", "tab_id": "t1", "focused": false}"#,
            r#"{"pane_id": "p2", "tab_id": "t2", "focused": false}"#,
            r#"{"pane_id": "p3", "tab_id": "t2", "focused": true}"#,
        ]
        .iter()
        .map(|s| s.parse().unwrap())
        .collect();
        assert_eq!(active_pane(&panes, "t1", false), Some("p1"));
        assert_eq!(active_pane(&panes, "t2", true), Some("p3"));
        assert_eq!(active_pane(&panes, "t2", false), None);
    }
}
