use nanoserde::{DeJson, SerJson};
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

/// Noise patterns to drop (matching the old promtail/lua config).
const DROP_PATTERNS: &[&str] = &[
    "ignored inotify event for",
    "PCC check channel failed for ss",
    "hwmon hwmon1: Undervoltage detected!",
    "hwmon hwmon1: Voltage normalised",
    "refused connection: IN=",
];

/// Input record from systemd journal via fluent-bit.
/// Uses default for missing fields so deserialization never fails on absent keys.
#[derive(DeJson, Default)]
#[nserde(default)]
struct JournalRecord {
    #[nserde(rename = "MESSAGE")]
    message: String,
    #[nserde(rename = "_HOSTNAME")]
    hostname: String,
    #[nserde(rename = "_SYSTEMD_UNIT")]
    systemd_unit: String,
    #[nserde(rename = "_TRANSPORT")]
    transport: String,
    #[nserde(rename = "PRIORITY")]
    priority: String,
    #[nserde(rename = "SYSLOG_IDENTIFIER")]
    syslog_identifier: String,
    #[nserde(rename = "_PID")]
    pid: String,
    #[nserde(rename = "CONTAINER_NAME")]
    container_name: String,
    #[nserde(rename = "COREDUMP_CGROUP")]
    coredump_cgroup: String,
    #[nserde(rename = "COREDUMP_EXE")]
    coredump_exe: String,
    #[nserde(rename = "COREDUMP_UID")]
    coredump_uid: String,
    #[nserde(rename = "COREDUMP_GID")]
    coredump_gid: String,
    #[nserde(rename = "COREDUMP_CMDLINE")]
    coredump_cmdline: String,
}

/// Output record sent to Loki.
#[derive(SerJson)]
struct OutputRecord {
    #[nserde(rename = "MESSAGE")]
    message: String,
    host: String,
    unit: String,
    coredump_unit: String,
    priority: String,
    syslog_identifier: String,
    pid: String,
    container_name: String,
}

/// Fluent-bit WASM filter entry point.
///
/// Returns a JSON string with the transformed record, or an empty string to
/// drop the record.
#[no_mangle]
pub extern "C" fn filter_journal(
    _tag: *const c_char,
    _tag_len: u32,
    _time_sec: u32,
    _time_nsec: u32,
    record: *const c_char,
    _record_len: u32,
) -> *const c_char {
    let record_str = unsafe { CStr::from_ptr(record) }.to_str().unwrap_or("");
    let result = match process_record(record_str) {
        Some(s) => s,
        None => String::new(), // empty = drop record
    };
    CString::new(result).unwrap().into_raw()
}

fn process_record(record_str: &str) -> Option<String> {
    let r: JournalRecord = DeJson::deserialize_json(record_str).ok()?;

    // Check MESSAGE for noise patterns.
    for pattern in DROP_PATTERNS {
        if r.message.contains(pattern) {
            return None;
        }
    }

    // Unit label: fall back to _TRANSPORT like promtail did.
    let unit = if !r.systemd_unit.is_empty() {
        collapse_session_scope(&r.systemd_unit)
    } else if !r.transport.is_empty() {
        r.transport.clone()
    } else {
        String::new()
    };

    // Coredump enrichment.
    let (final_msg, coredump_unit) = if !r.coredump_cgroup.is_empty() {
        let cu = r.coredump_cgroup.rsplit('/').next().unwrap_or("");
        let exe = if r.coredump_exe.is_empty() {
            "?"
        } else {
            &r.coredump_exe
        };
        let uid = if r.coredump_uid.is_empty() {
            "?"
        } else {
            &r.coredump_uid
        };
        let gid = if r.coredump_gid.is_empty() {
            "?"
        } else {
            &r.coredump_gid
        };
        let cmd = if r.coredump_cmdline.is_empty() {
            "?"
        } else {
            &r.coredump_cmdline
        };
        (
            format!("{exe} core dumped (user: {uid}/{gid}, command: {cmd})"),
            cu.to_string(),
        )
    } else {
        (r.message.clone(), String::new())
    };

    let out = OutputRecord {
        message: final_msg,
        host: r.hostname,
        unit,
        coredump_unit,
        priority: r.priority,
        syslog_identifier: r.syslog_identifier,
        pid: r.pid,
        container_name: r.container_name,
    };

    Some(SerJson::serialize_json(&out))
}

/// Collapse session-1234.scope → session.scope to keep label cardinality low.
fn collapse_session_scope(unit: &str) -> String {
    if unit.starts_with("session-") && unit.ends_with(".scope") {
        let middle = &unit[8..unit.len() - 6];
        if middle.chars().all(|c| c.is_ascii_digit()) {
            return "session.scope".to_string();
        }
    }
    unit.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_record(fields: &[(&str, &str)]) -> String {
        let pairs: Vec<String> = fields
            .iter()
            .map(|(k, v)| format!("\"{}\":\"{}\"", k, v))
            .collect();
        format!("{{{}}}", pairs.join(","))
    }

    #[test]
    fn test_normal_record() {
        let json = make_record(&[
            ("MESSAGE", "hello world"),
            ("_HOSTNAME", "node1"),
            ("_SYSTEMD_UNIT", "sshd.service"),
            ("PRIORITY", "6"),
            ("SYSLOG_IDENTIFIER", "sshd"),
            ("_PID", "1234"),
        ]);
        let result = process_record(&json).unwrap();
        assert!(result.contains("\"MESSAGE\":\"hello world\""));
        assert!(result.contains("\"unit\":\"sshd.service\""));
        assert!(result.contains("\"host\":\"node1\""));
    }

    #[test]
    fn test_noise_drop() {
        let json = make_record(&[
            ("MESSAGE", "some ignored inotify event for blah"),
            ("_HOSTNAME", "node1"),
        ]);
        assert!(process_record(&json).is_none());
    }

    #[test]
    fn test_session_scope_collapse() {
        assert_eq!(collapse_session_scope("session-42.scope"), "session.scope");
        assert_eq!(
            collapse_session_scope("session-abc.scope"),
            "session-abc.scope"
        );
        assert_eq!(collapse_session_scope("sshd.service"), "sshd.service");
    }

    #[test]
    fn test_transport_fallback() {
        let json = make_record(&[
            ("MESSAGE", "kernel message"),
            ("_HOSTNAME", "node1"),
            ("_TRANSPORT", "kernel"),
        ]);
        let result = process_record(&json).unwrap();
        assert!(result.contains("\"unit\":\"kernel\""));
    }

    #[test]
    fn test_coredump_enrichment() {
        let json = make_record(&[
            ("MESSAGE", "original"),
            ("_HOSTNAME", "node1"),
            ("_SYSTEMD_UNIT", "foo.service"),
            ("COREDUMP_CGROUP", "/system.slice/foo.service"),
            ("COREDUMP_EXE", "/usr/bin/foo"),
            ("COREDUMP_UID", "1000"),
            ("COREDUMP_GID", "1000"),
            ("COREDUMP_CMDLINE", "foo --bar"),
        ]);
        let result = process_record(&json).unwrap();
        assert!(result.contains("/usr/bin/foo core dumped (user: 1000/1000, command: foo --bar)"));
        assert!(result.contains("\"coredump_unit\":\"foo.service\""));
    }
}
