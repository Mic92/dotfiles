package main

import (
	"encoding/json"
	"testing"
)

func TestNormalRecord(t *testing.T) {
	input := `{"MESSAGE":"hello world","_HOSTNAME":"node1","_SYSTEMD_UNIT":"sshd.service","PRIORITY":"6","SYSLOG_IDENTIFIER":"sshd","_PID":"1234"}`
	result := processRecord(input)
	if result == "" {
		t.Fatal("expected output, got drop")
	}
	var out outputRecord
	if err := json.Unmarshal([]byte(result), &out); err != nil {
		t.Fatal(err)
	}
	if out.Message != "hello world" {
		t.Errorf("message = %q, want %q", out.Message, "hello world")
	}
	if out.Unit != "sshd.service" {
		t.Errorf("unit = %q, want %q", out.Unit, "sshd.service")
	}
	if out.Host != "node1" {
		t.Errorf("host = %q, want %q", out.Host, "node1")
	}
}

func TestNoiseDrop(t *testing.T) {
	input := `{"MESSAGE":"some ignored inotify event for blah","_HOSTNAME":"node1"}`
	if result := processRecord(input); result != "" {
		t.Errorf("expected drop, got %q", result)
	}
}

func TestSessionScopeCollapse(t *testing.T) {
	tests := []struct {
		in, want string
	}{
		{"session-42.scope", "session.scope"},
		{"session-abc.scope", "session-abc.scope"},
		{"sshd.service", "sshd.service"},
		{"session-.scope", "session-.scope"},
	}
	for _, tt := range tests {
		got := collapseSessionScope(tt.in)
		if got != tt.want {
			t.Errorf("collapseSessionScope(%q) = %q, want %q", tt.in, got, tt.want)
		}
	}
}

func TestTransportFallback(t *testing.T) {
	input := `{"MESSAGE":"kernel message","_HOSTNAME":"node1","_TRANSPORT":"kernel"}`
	result := processRecord(input)
	var out outputRecord
	if err := json.Unmarshal([]byte(result), &out); err != nil {
		t.Fatal(err)
	}
	if out.Unit != "kernel" {
		t.Errorf("unit = %q, want %q", out.Unit, "kernel")
	}
}

func TestCoredumpEnrichment(t *testing.T) {
	input := `{"MESSAGE":"original","_HOSTNAME":"node1","_SYSTEMD_UNIT":"foo.service","COREDUMP_CGROUP":"/system.slice/foo.service","COREDUMP_EXE":"/usr/bin/foo","COREDUMP_UID":"1000","COREDUMP_GID":"1000","COREDUMP_CMDLINE":"foo --bar"}`
	result := processRecord(input)
	var out outputRecord
	if err := json.Unmarshal([]byte(result), &out); err != nil {
		t.Fatal(err)
	}
	want := "/usr/bin/foo core dumped (user: 1000/1000, command: foo --bar)"
	if out.Message != want {
		t.Errorf("message = %q, want %q", out.Message, want)
	}
	if out.CoredumpUnit != "foo.service" {
		t.Errorf("coredump_unit = %q, want %q", out.CoredumpUnit, "foo.service")
	}
}
