//! Running helper programs without blocking on them or leaking zombies.

use std::process::{Command, Stdio};
use std::thread;

/// noctalia's IPC; shows its OSD for volume, brightness and friends.
pub fn noctalia(args: &[&str]) {
    let mut full = vec!["msg"];
    full.extend_from_slice(args);
    spawn("noctalia", &full);
}

/// Stdout of a command that must finish quickly, trimmed; `None` on failure.
pub fn output(cmd: &str, args: &[&str]) -> Option<String> {
    let out = Command::new(cmd)
        .args(args)
        .stdin(Stdio::null())
        .stderr(Stdio::null())
        .output()
        .ok()?;
    let text = String::from_utf8_lossy(&out.stdout).trim().to_owned();
    (out.status.success() && !text.is_empty()).then_some(text)
}

pub fn spawn(cmd: &str, args: &[&str]) {
    match Command::new(cmd).args(args).stdin(Stdio::null()).spawn() {
        // Reaped by a detached thread so finished children never pile up.
        Ok(mut child) => drop(thread::spawn(move || child.wait())),
        Err(e) => eprintln!("voice-assistant: {cmd}: {e}"),
    }
}

/// Run to completion, quietly; whether it exited successfully.
pub fn succeeds(cmd: &str, args: &[&str]) -> bool {
    match Command::new(cmd)
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
    {
        Ok(s) => s.success(),
        Err(e) => {
            eprintln!("voice-assistant: {cmd}: {e}");
            false
        }
    }
}
