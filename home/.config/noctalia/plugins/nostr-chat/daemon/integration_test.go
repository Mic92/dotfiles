package main

import (
	"context"
	"encoding/json"
	"net"
	"net/http/httptest"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"fiatjaf.com/nostr"
	"fiatjaf.com/nostr/eventstore/slicestore"
	"fiatjaf.com/nostr/khatru"
)

// startTestRelay spins up an in-process khatru relay backed by an
// in-memory slice store. Lifted from opencrow's testutil — small enough
// to copy rather than pull in a cross-repo dependency.
func startTestRelay(t *testing.T) string {
	t.Helper()
	relay := khatru.NewRelay()
	store := &slicestore.SliceStore{}
	if err := store.Init(); err != nil {
		t.Fatalf("init slice store: %v", err)
	}
	relay.UseEventstore(store, 500)
	srv := httptest.NewServer(relay)
	t.Cleanup(srv.Close)
	return "ws" + strings.TrimPrefix(srv.URL, "http")
}

// harness runs a Daemon against a test relay with push events captured
// on a channel. Commands go in via the real unix socket so the IPC
// framing is exercised end-to-end.
type harness struct {
	d      *Daemon
	keys   Keys
	events chan Event
	sock   string
	cancel context.CancelFunc
}

func newHarness(t *testing.T, relay, peer string) *harness {
	t.Helper()
	dir := t.TempDir()
	sk := nostr.Generate()
	keys := Keys{SK: sk, PK: nostr.GetPublicKey(sk)}
	cfg := Config{
		PeerPubKey: peer,
		Relays:    []string{relay},
		Name:      "test",
		Socket:    filepath.Join(dir, "sock"),
		StateDir:  filepath.Join(dir, "state"),
		CacheDir:  filepath.Join(dir, "cache"),
	}
	store, err := OpenStore(cfg.StateDir)
	if err != nil {
		t.Fatalf("open store: %v", err)
	}
	t.Cleanup(func() { store.Close() })

	events := make(chan Event, 64)
	d := NewDaemon(cfg, keys, store, func(ev Event) { events <- ev })

	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	go d.Run(ctx)

	h := &harness{d: d, keys: keys, events: events, sock: cfg.Socket, cancel: cancel}
	// Boot status is the first event — consume it so tests start clean,
	// and it doubles as a readiness signal for the socket.
	ev := h.expect(t, EvStatus, 5*time.Second)
	if !ev.Booted {
		t.Fatalf("first status not Booted: %+v", ev)
	}
	return h
}

// send writes one Command to the daemon's socket, the same way the QML
// side does: connect, one NDJSON line, close.
func (h *harness) send(t *testing.T, c Command) {
	t.Helper()
	// serveSocket may not have bound yet on the very first call.
	var conn net.Conn
	var err error
	for i := 0; i < 50; i++ {
		conn, err = net.Dial("unix", h.sock)
		if err == nil {
			break
		}
		time.Sleep(10 * time.Millisecond)
	}
	if err != nil {
		t.Fatalf("dial socket: %v", err)
	}
	defer conn.Close()
	b, _ := json.Marshal(c)
	if _, err := conn.Write(append(b, '\n')); err != nil {
		t.Fatalf("write socket: %v", err)
	}
}

// expect pulls events until one of the given kind arrives, or times
// out. Intermediate events are dropped — tests only assert on the ones
// they care about, so ordering jitter between e.g. EvMsg and EvSent
// doesn't make them flaky.
func (h *harness) expect(t *testing.T, kind EventKind, d time.Duration) Event {
	t.Helper()
	deadline := time.After(d)
	for {
		select {
		case ev := <-h.events:
			if ev.Kind == kind {
				return ev
			}
		case <-deadline:
			t.Fatalf("timed out waiting for %s event", kind)
			return Event{}
		}
	}
}

// TestSendRoundTrip: write a send command to the socket, verify the
// local echo, the peer receiving the gift-wrap, and the EvSent
// confirmation once the relay accepts.
func TestSendRoundTrip(t *testing.T) {
	t.Parallel()
	relay := startTestRelay(t)

	// Peer listens first so its pubkey can be the daemon's PeerPubKey.
	peerSK := nostr.Generate()
	peerKeys := Keys{SK: peerSK, PK: nostr.GetPublicKey(peerSK)}
	peer := NewListener(peerKeys, []string{relay})

	h := newHarness(t, relay, peerKeys.PK.Hex())

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	peerCh := make(chan Rumor, 4)
	go peer.Run(ctx, func() int64 { return 0 }, peerCh)

	h.send(t, Command{Cmd: CmdSend, Text: "hello peer"})

	// Local echo: EvMsg with our pubkey, pending state.
	echo := h.expect(t, EvMsg, 5*time.Second)
	if echo.Msg == nil || echo.Msg.Content != "hello peer" {
		t.Fatalf("echo = %+v", echo)
	}
	if echo.Msg.Dir != DirOut || echo.Msg.State != StatePending {
		t.Errorf("echo dir/state = %s/%s, want out/pending", echo.Msg.Dir, echo.Msg.State)
	}
	rumorID := echo.Msg.ID

	// Peer receives the unwrapped rumor.
	var got Rumor
	select {
	case got = <-peerCh:
	case <-time.After(5 * time.Second):
		t.Fatal("peer did not receive rumor")
	}
	if got.Content != "hello peer" || got.Kind != 14 {
		t.Errorf("peer got %+v", got)
	}
	if got.PubKey != h.keys.PK.Hex() {
		t.Errorf("peer saw pubkey %s, want %s", got.PubKey, h.keys.PK.Hex())
	}
	if got.ID != rumorID {
		t.Errorf("peer rumor id %s != echo id %s", got.ID, rumorID)
	}

	// publishLoop confirms relay acceptance.
	sent := h.expect(t, EvSent, 5*time.Second)
	if sent.Target != rumorID || sent.State != StateSent {
		t.Errorf("sent event = %+v, want target=%s state=sent", sent, rumorID)
	}
}

// TestSendWhileOffline: send with no relay reachable. Must echo
// locally, surface one EvRetry, and not inflate tries — so the item
// fires immediately once a relay appears instead of sitting in backoff.
func TestSendWhileOffline(t *testing.T) {
	t.Parallel()

	peerSK := nostr.Generate()
	peerPK := nostr.GetPublicKey(peerSK)

	// Dead port — subscription never connects, publishConnected skips.
	h := newHarness(t, "ws://127.0.0.1:1", peerPK.Hex())

	h.send(t, Command{Cmd: CmdSend, Text: "queued"})

	echo := h.expect(t, EvMsg, 5*time.Second)
	if echo.Msg == nil || echo.Msg.State != StatePending {
		t.Fatalf("echo = %+v, want pending", echo)
	}

	retry := h.expect(t, EvRetry, 5*time.Second)
	if retry.Tries != 1 {
		t.Errorf("tries = %d, want 1 (defer surfaces once)", retry.Tries)
	}

	// No second EvRetry — defer is silent after the first.
	select {
	case ev := <-h.events:
		if ev.Kind == EvRetry {
			t.Fatalf("unexpected second EvRetry: %+v", ev)
		}
	case <-time.After(300 * time.Millisecond):
	}

	// Outbox row should sit at tries=1, next_at=0 — ready to fire the
	// instant a relay comes back rather than stuck in exponential
	// backoff after N ticks offline.
	items, err := h.d.store.PendingOutbox(context.Background(), time.Now().Unix())
	if err != nil {
		t.Fatalf("outbox: %v", err)
	}
	if len(items) != 1 || items[0].Tries != 1 {
		t.Errorf("outbox = %+v, want 1 item at tries=1", items)
	}
}

// TestIncomingMessage: peer publishes a DM, daemon surfaces it as an
// EvMsg with dir=in and stores it for replay.
func TestIncomingMessage(t *testing.T) {
	t.Parallel()
	relay := startTestRelay(t)

	peerSK := nostr.Generate()
	peerKeys := Keys{SK: peerSK, PK: nostr.GetPublicKey(peerSK)}
	peer := NewListener(peerKeys, []string{relay})

	h := newHarness(t, relay, peerKeys.PK.Hex())

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	out, err := peer.Prepare(ctx, h.keys.PK.Hex(), "ping from peer", "")
	if err != nil {
		t.Fatalf("peer prepare: %v", err)
	}
	if err := peer.Publish(ctx, out); err != nil {
		t.Fatalf("peer publish: %v", err)
	}

	ev := h.expect(t, EvMsg, 5*time.Second)
	if ev.Msg == nil || ev.Msg.Content != "ping from peer" {
		t.Fatalf("incoming msg = %+v", ev)
	}
	if ev.Msg.Dir != DirIn {
		t.Errorf("dir = %s, want in", ev.Msg.Dir)
	}
	if ev.Msg.PubKey != peerKeys.PK.Hex() {
		t.Errorf("pubkey = %s, want %s", ev.Msg.PubKey, peerKeys.PK.Hex())
	}

	// Replay should return the stored message.
	h.send(t, Command{Cmd: CmdReplay, N: 10})
	h.expect(t, EvStatus, 5*time.Second) // replay leads with a status
	replayed := h.expect(t, EvMsg, 5*time.Second)
	if replayed.Msg == nil || replayed.Msg.ID != ev.Msg.ID {
		t.Errorf("replayed %+v, want id %s", replayed, ev.Msg.ID)
	}
}

// TestStrangerIgnored: a DM from a pubkey that is neither us nor the
// configured peer must not produce an EvMsg. Guards against the
// github-notifier-spams-our-p-tag failure mode noted in handleRumor.
func TestStrangerIgnored(t *testing.T) {
	t.Parallel()
	relay := startTestRelay(t)

	peerSK := nostr.Generate()
	peerKeys := Keys{SK: peerSK, PK: nostr.GetPublicKey(peerSK)}

	h := newHarness(t, relay, peerKeys.PK.Hex())

	strangerSK := nostr.Generate()
	strangerKeys := Keys{SK: strangerSK, PK: nostr.GetPublicKey(strangerSK)}
	stranger := NewListener(strangerKeys, []string{relay})

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	out, err := stranger.Prepare(ctx, h.keys.PK.Hex(), "spam", "")
	if err != nil {
		t.Fatalf("stranger prepare: %v", err)
	}
	if err := stranger.Publish(ctx, out); err != nil {
		t.Fatalf("stranger publish: %v", err)
	}

	select {
	case ev := <-h.events:
		if ev.Kind == EvMsg {
			t.Fatalf("stranger message leaked through: %+v", ev)
		}
	case <-time.After(500 * time.Millisecond):
		// good — nothing surfaced
	}
}
