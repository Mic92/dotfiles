package main

import (
	"encoding/json"
	"strings"
	"unsafe"
)

// Noise patterns to drop.
var dropPatterns = []string{
	"ignored inotify event for",
	"PCC check channel failed for ss",
	"hwmon hwmon1: Undervoltage detected!",
	"hwmon hwmon1: Voltage normalised",
	"refused connection: IN=",
}

// journalRecord represents systemd journal fields from fluent-bit.
type journalRecord struct {
	Message          string `json:"MESSAGE"`
	Hostname         string `json:"_HOSTNAME"`
	SystemdUnit      string `json:"_SYSTEMD_UNIT"`
	Transport        string `json:"_TRANSPORT"`
	Priority         string `json:"PRIORITY"`
	SyslogIdentifier string `json:"SYSLOG_IDENTIFIER"`
	PID              string `json:"_PID"`
	ContainerName    string `json:"CONTAINER_NAME"`
	CoredumpCgroup   string `json:"COREDUMP_CGROUP"`
	CoredumpExe      string `json:"COREDUMP_EXE"`
	CoredumpUID      string `json:"COREDUMP_UID"`
	CoredumpGID      string `json:"COREDUMP_GID"`
	CoredumpCmdline  string `json:"COREDUMP_CMDLINE"`
}

// outputRecord is sent to Loki.
type outputRecord struct {
	Message          string `json:"MESSAGE"`
	Host             string `json:"host"`
	Unit             string `json:"unit"`
	CoredumpUnit     string `json:"coredump_unit"`
	Priority         string `json:"priority"`
	SyslogIdentifier string `json:"syslog_identifier"`
	PID              string `json:"pid"`
	ContainerName    string `json:"container_name"`
}

//export go_filter
func go_filter(tag *uint8, tag_len uint, time_sec uint, time_nsec uint, record *uint8, record_len uint) *uint8 {
	brecord := unsafe.Slice(record, record_len)

	result := processRecord(string(brecord))
	if result == "" {
		return nil // drop record
	}

	// Must be null-terminated for fluent-bit.
	buf := make([]byte, len(result)+1)
	copy(buf, result)
	buf[len(result)] = 0
	return &buf[0]
}

func processRecord(raw string) string {
	var r journalRecord
	if err := json.Unmarshal([]byte(raw), &r); err != nil {
		return ""
	}

	// Drop noise.
	for _, pattern := range dropPatterns {
		if strings.Contains(r.Message, pattern) {
			return ""
		}
	}

	// Unit label: fall back to _TRANSPORT like promtail did.
	unit := r.SystemdUnit
	if unit == "" {
		unit = r.Transport
	}
	unit = collapseSessionScope(unit)

	// Coredump enrichment.
	msg := r.Message
	coredumpUnit := ""
	if r.CoredumpCgroup != "" {
		parts := strings.Split(r.CoredumpCgroup, "/")
		coredumpUnit = parts[len(parts)-1]
		exe := defaultStr(r.CoredumpExe, "?")
		uid := defaultStr(r.CoredumpUID, "?")
		gid := defaultStr(r.CoredumpGID, "?")
		cmd := defaultStr(r.CoredumpCmdline, "?")
		msg = exe + " core dumped (user: " + uid + "/" + gid + ", command: " + cmd + ")"
	}

	out := outputRecord{
		Message:          msg,
		Host:             r.Hostname,
		Unit:             unit,
		CoredumpUnit:     coredumpUnit,
		Priority:         r.Priority,
		SyslogIdentifier: r.SyslogIdentifier,
		PID:              r.PID,
		ContainerName:    r.ContainerName,
	}

	b, err := json.Marshal(out)
	if err != nil {
		return ""
	}
	return string(b)
}

// collapseSessionScope collapses session-1234.scope → session.scope
// to keep label cardinality low.
func collapseSessionScope(unit string) string {
	if strings.HasPrefix(unit, "session-") && strings.HasSuffix(unit, ".scope") {
		middle := unit[8 : len(unit)-6]
		allDigits := true
		for _, c := range middle {
			if c < '0' || c > '9' {
				allDigits = false
				break
			}
		}
		if allDigits && len(middle) > 0 {
			return "session.scope"
		}
	}
	return unit
}

func defaultStr(s, fallback string) string {
	if s == "" {
		return fallback
	}
	return s
}

func main() {}
