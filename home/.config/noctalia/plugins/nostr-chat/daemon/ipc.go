package main

import (
	"bufio"
	"context"
	"encoding/json"
	"log/slog"
	"net"
	"os"
	"os/exec"
	"time"
)

// Shell-bound event. Kinds:
//   status  — streaming bool, pubkey, unread count (sent on connect and on replay)
//   msg     — a Message (new or replayed)
//   ack     — reaction landed on one of our outgoing messages
//   img     — async download finished; attach local path to a prior msg
//   error   — something worth a toast
type Event struct {
	Kind   string   `json:"kind"`
	Msg    *Message `json:"msg,omitempty"`
	Target string   `json:"target,omitempty"` // ack/img: rumor id
	Mark   string   `json:"mark,omitempty"`   // ack: reaction content
	Image  string   `json:"image,omitempty"`  // img: local file path
	// status fields
	Streaming bool   `json:"streaming,omitempty"`
	Booted    bool   `json:"booted,omitempty"` // true only on the daemon's first status push
	PubKey    string `json:"pubkey,omitempty"`
	Name      string `json:"name,omitempty"` // display label for the panel header
	Unread    int    `json:"unread,omitempty"`
	Text      string `json:"text,omitempty"` // error
}

// pushIPC forks quickshell to inject one event into the plugin's
// IpcHandler. Fire-and-forget: if the shell isn't running the exec
// fails and we move on — the message is in sqlite, the shell will ask
// for a replay when it comes back.
func pushIPC(ev Event) {
	b, _ := json.Marshal(ev)
	cmd := exec.Command("noctalia-shell", "ipc", "call", "plugin:nostr-chat", "recv", string(b))
	cmd.Stdout = nil
	cmd.Stderr = nil
	// Don't block the listen loop on a slow shell.
	go func() {
		if err := cmd.Run(); err != nil {
			slog.Debug("ipc push dropped (shell down?)", "err", err)
		}
	}()
}

// Command from the shell over the unix socket.
type Command struct {
	Cmd  string `json:"cmd"`
	Text    string `json:"text,omitempty"`    // send
	ReplyTo string `json:"replyTo,omitempty"` // send: e-tag target (rumor id)
	Path    string `json:"path,omitempty"`    // send-file: local file to upload
	N    int    `json:"n,omitempty"`    // replay: how many recent messages
}

// serveSocket accepts one-shot connections: read one NDJSON line, act,
// close. Keeps the QML side trivially simple — no keepalive, no
// reconnect logic, just Socket{connected:true; write(...); connected:false}.
func serveSocket(ctx context.Context, path string, handle func(Command)) error {
	_ = os.Remove(path)
	ln, err := net.Listen("unix", path)
	if err != nil {
		return err
	}
	go func() { <-ctx.Done(); ln.Close() }()
	for {
		conn, err := ln.Accept()
		if err != nil {
			if ctx.Err() != nil {
				return nil
			}
			slog.Warn("accept", "err", err)
			continue
		}
		go func(c net.Conn) {
			defer c.Close()
			c.SetDeadline(time.Now().Add(5 * time.Second))
			sc := bufio.NewScanner(c)
			for sc.Scan() {
				var cmd Command
				if err := json.Unmarshal(sc.Bytes(), &cmd); err != nil {
					slog.Warn("bad command", "raw", sc.Text(), "err", err)
					continue
				}
				handle(cmd)
			}
		}(conn)
	}
}
