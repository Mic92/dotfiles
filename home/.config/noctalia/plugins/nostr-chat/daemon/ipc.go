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

// EventKind tags what the shell should do with a pushed Event. Typed
// so adding a new kind forces touching both the push sites and this
// list, instead of stringly drifting.
type EventKind string

const (
	EvStatus EventKind = "status" // streaming bool, pubkey, unread count
	EvMsg    EventKind = "msg"    // a Message (new or replayed)
	EvSent   EventKind = "sent"   // relay accepted, or State=Cancelled if user dropped it
	EvRetry  EventKind = "retry"  // publish failed, backing off; Tries + error text
	EvAck    EventKind = "ack"    // kind-7 reaction landed on one of our outgoing messages
	EvImg    EventKind = "img"    // async download finished; attach local path to a prior msg
	EvError  EventKind = "error"  // something worth a toast
)

type Event struct {
	Kind   EventKind `json:"kind"`
	Msg    *Message  `json:"msg,omitempty"`
	Target string    `json:"target,omitempty"` // ack/img/sent/retry: rumor id
	Mark   string    `json:"mark,omitempty"`   // ack: reaction content
	Image  string    `json:"image,omitempty"`  // img: local file path
	State  State     `json:"state,omitempty"`  // sent
	Tries  int       `json:"tries,omitempty"`  // retry: attempt count
	// status fields
	Streaming bool   `json:"streaming,omitempty"`
	Booted    bool   `json:"booted,omitempty"` // true only on the daemon's first status push
	PubKey    string `json:"pubkey,omitempty"`
	Name      string `json:"name,omitempty"` // display label for the panel header
	Unread    int    `json:"unread,omitempty"`
	Text      string `json:"text,omitempty"` // error
}

// PushFunc delivers an Event to the shell. Abstracted so tests can
// capture events instead of forking noctalia-shell.
type PushFunc func(Event)

// shellPush forks quickshell to inject one event into the plugin's
// IpcHandler. Fire-and-forget: if the shell isn't running the exec
// fails and we move on — the message is in sqlite, the shell will ask
// for a replay when it comes back.
func shellPush(ev Event) {
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

// Cmd names a socket command. Same rationale as EventKind: the switch
// in handleCommand is the only consumer, so a typo here fails loudly
// at the default branch instead of silently doing nothing.
type Cmd string

const (
	CmdSend     Cmd = "send"
	CmdSendFile Cmd = "send-file"
	CmdReplay   Cmd = "replay"
	CmdMarkRead Cmd = "mark-read"
	CmdRetry    Cmd = "retry"
	CmdCancel   Cmd = "cancel"
)

// Command from the shell over the unix socket.
type Command struct {
	Cmd     Cmd    `json:"cmd"`
	Text    string `json:"text,omitempty"`    // send
	ReplyTo string `json:"replyTo,omitempty"` // send: e-tag target (rumor id)
	Path    string `json:"path,omitempty"`    // send-file: local file to upload
	Unlink  bool   `json:"unlink,omitempty"`  // send-file: remove Path after caching (for mktemp sources)
	N       int    `json:"n,omitempty"`       // replay: how many recent messages
	ID      string `json:"id,omitempty"`      // retry/cancel: rumor id
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
	slog.Info("socket listening", "path", path)
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
