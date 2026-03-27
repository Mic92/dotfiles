// nostr-chatd bridges a noctalia-shell chat panel to a Nostr NIP-17 peer.
//
// It replaces a brittle QML→Python→nak pipeline with a single process
// that links fiatjaf.com/nostr directly, persists history in sqlite,
// and pushes events into the shell via `quickshell ipc call`. The shell
// talks back over a unix socket with one-line JSON commands.
//
// Design: the daemon is the source of truth. Shell restarts are free
// (it asks for a replay), daemon restarts are cheap (sqlite dedup +
// outbox), and nothing is lost if either side is down when a message
// arrives or is sent.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"log/slog"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"fiatjaf.com/nostr"
)

type Config struct {
	PeerPubKey string
	Relays    []string
	Blossom   []string // upload targets for send-file
	SecretCmd string // shell command that prints the nsec to stdout
	Name      string // display label pushed to the shell in status events
	Socket    string
	StateDir  string
	CacheDir  string // downloaded attachments
}

func xdg(env, fallback string) string {
	if v := os.Getenv(env); v != "" {
		return v
	}
	home, _ := os.UserHomeDir()
	return filepath.Join(home, fallback)
}

func loadConfig() Config {
	runtime := os.Getenv("XDG_RUNTIME_DIR")
	if runtime == "" {
		runtime = fmt.Sprintf("/run/user/%d", os.Getuid())
	}
	c := Config{
		PeerPubKey: os.Getenv("NOSTR_CHAT_PEER_PUBKEY"),
		SecretCmd: os.Getenv("NOSTR_CHAT_SECRET_CMD"),
		Name:      envOr("NOSTR_CHAT_DISPLAY_NAME", "Chat"),
		Socket:    filepath.Join(runtime, "nostr-chatd.sock"),
		StateDir:  filepath.Join(xdg("XDG_STATE_HOME", ".local/state"), "nostr-chatd"),
		CacheDir:  filepath.Join(xdg("XDG_CACHE_HOME", ".cache"), "nostr-chatd", "media"),
	}
	for _, r := range strings.Split(os.Getenv("NOSTR_CHAT_RELAYS"), ",") {
		if r = strings.TrimSpace(r); r != "" {
			c.Relays = append(c.Relays, r)
		}
	}
	for _, r := range strings.Split(os.Getenv("NOSTR_CHAT_BLOSSOM"), ",") {
		if r = strings.TrimSpace(r); r != "" {
			c.Blossom = append(c.Blossom, r)
		}
	}
	flag.StringVar(&c.Socket, "socket", c.Socket, "unix socket path")
	flag.StringVar(&c.StateDir, "state", c.StateDir, "sqlite state dir")
	flag.Parse()
	return c
}

func envOr(k, d string) string {
	if v := os.Getenv(k); v != "" {
		return v
	}
	return d
}

// fetchSecret runs a shell command and returns its trimmed stdout.
// `sh -c` lets the module pass anything — `rbw get foo`, `pass show
// nostr/identity`, `cat /run/agenix/nsec` — without the daemon caring
// which backend is in play.
func fetchSecret(shcmd string) (string, error) {
	if shcmd == "" {
		return "", fmt.Errorf("NOSTR_CHAT_SECRET_CMD is empty")
	}
	// /bin/sh is one of the few FHS paths NixOS guarantees — using it
	// directly means the systemd unit's PATH only needs the tools the
	// secret command itself calls, not a shell.
	out, err := exec.Command("/bin/sh", "-c", shcmd).Output()
	if err != nil {
		var ee *exec.ExitError
		if errors.As(err, &ee) {
			return "", fmt.Errorf("secret command failed: %w: %s", err, ee.Stderr)
		}
		return "", fmt.Errorf("secret command: %w", err)
	}
	return strings.TrimSpace(string(out)), nil
}

func main() {
	slog.SetDefault(slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: slog.LevelDebug})))
	cfg := loadConfig()

	if cfg.PeerPubKey == "" || len(cfg.Relays) == 0 {
		slog.Error("missing NOSTR_CHAT_PEER_PUBKEY or NOSTR_CHAT_RELAYS")
		os.Exit(1)
	}

	sec, err := fetchSecret(cfg.SecretCmd)
	if err != nil {
		slog.Error("fetch secret", "err", err)
		os.Exit(1)
	}
	keys, err := loadKeys(sec)
	if err != nil {
		slog.Error("load keys", "err", err)
		os.Exit(1)
	}
	slog.Info("identity", "pubkey", keys.PK.Hex())

	store, err := OpenStore(cfg.StateDir)
	if err != nil {
		slog.Error("open store", "err", err)
		os.Exit(1)
	}
	defer store.Close()

	ctx, cancel := signal.NotifyContext(context.Background(), syscall.SIGTERM, syscall.SIGINT)
	defer cancel()

	lst := NewListener(keys, cfg.Relays)

	// Rumors and commands funnel through this goroutine so the common
	// path (text in/out) never contends on the store. File upload and
	// download spawn their own goroutines and hit sqlite directly —
	// safe because modernc.org/sqlite serialises via busy_timeout, and
	// the only shared row is the rumor id whose ON CONFLICT handles
	// the race.
	rumors := make(chan Rumor, 64)
	cmds := make(chan Command, 16)

	// Anything timestamped before we started is backfill, not a live
	// reply — mark it read so the shell doesn't auto-open the panel for
	// three days of history on first run.
	startedAt := time.Now().Unix()
	// Closure so reconnects pick up the watermark handleRumor has been
	// advancing, instead of replaying from the boot-time value.
	go lst.Run(ctx, func() int64 {
		ts, _ := store.GetInt(ctx, "last_seen_ts")
		return ts
	}, rumors)

	go func() {
		if err := serveSocket(ctx, cfg.Socket, func(c Command) { cmds <- c }); err != nil {
			slog.Error("socket", "err", err)
			cancel()
		}
	}()

	// Publishing blocks on slow/dead relays (nostr.0cx.de takes the full
	// 15s timeout when down). Run it in its own goroutine so a stalled
	// publish can't delay the next send's local echo or an incoming
	// rumor's IPC push. The main loop only does fast work: sqlite +
	// crypto + exec.
	drainNow := make(chan struct{}, 1)
	drainNow <- struct{}{} // flush anything a prior crash left behind
	go publishLoop(ctx, store, lst, cfg, drainNow)

	// Booted:true tells the shell this is a fresh daemon, not a replay
	// response — it should request backfill. Without the flag the shell
	// can't distinguish a restart from its own replay echo and either
	// misses history or loops forever.
	pushIPC(Event{Kind: "status", Streaming: true, Booted: true, PubKey: keys.PK.Hex(), Name: cfg.Name})

	for {
		select {
		case <-ctx.Done():
			return
		case r := <-rumors:
			handleRumor(ctx, store, cfg, keys, r, startedAt)
		case c := <-cmds:
			handleCommand(ctx, store, lst, cfg, keys, c, drainNow)
		}
	}
}

func handleRumor(ctx context.Context, store *Store, cfg Config, keys Keys, r Rumor, startedAt int64) {
	switch r.Kind {
	case nostr.KindReaction:
		// Gift-wrapped kind-7 reaction — read receipt. Only trust the
		// configured peer; anyone can p-tag us.
		if r.PubKey != cfg.PeerPubKey || r.ETag == "" {
			return
		}
		mark := r.Content
		if mark == "" || mark == "+" {
			mark = "✓"
		}
		if err := store.SetAck(ctx, r.ETag, mark); err != nil {
			slog.Warn("set ack", "err", err)
			return
		}
		pushIPC(Event{Kind: "ack", Target: r.ETag, Mark: mark})

	default:
		// kind 14 chat, kind 15 file, or anything else the peer wraps —
		// store them all, the UI only cares about in/out.
		mine := r.PubKey == keys.PK.Hex()
		fromPeer := r.PubKey == cfg.PeerPubKey
		if !mine && !fromPeer {
			return // e.g. github-notifier spamming our p-tag
		}
		content := r.Content
		if r.Kind == KindFileMessage {
			mt := tagValue(r.Tags, "file-type")
			content = fmt.Sprintf("📎 %s", mt)
			if mt == "" {
				content = "📎 file"
			}
		}
		m := Message{
			ID: r.ID, PubKey: r.PubKey, Content: content, TS: r.TS,
			Dir:     map[bool]string{true: "out", false: "in"}[mine],
			Read:    mine || r.TS < startedAt,
			ReplyTo: r.ETag, // kind-14 e-tag = threaded reply target
		}
		inserted, err := store.InsertMessage(ctx, m)
		if err != nil {
			slog.Warn("insert", "err", err)
			return
		}
		if !inserted {
			return // dedup hit — replay after restart
		}
		if r.TS > 0 {
			_ = store.SetInt(ctx, "last_seen_ts", r.TS)
		}
		pushIPC(Event{Kind: "msg", Msg: &m})

		// kind-15: download runs off the main loop so a slow Blossom
		// server can't stall incoming text. When done, patch the row
		// and push an "img" event — the shell updates the bubble in
		// place, same pattern as acks.
		if r.Kind == KindFileMessage {
			go func(r Rumor) {
				p, err := downloadFile(ctx, r.Content, cfg.CacheDir, r.Tags)
				if err != nil {
					slog.Warn("download", "url", r.Content, "err", err)
					return
				}
				if err := store.SetImage(ctx, r.ID, p); err != nil {
					slog.Warn("set image", "err", err)
				}
				pushIPC(Event{Kind: "img", Target: r.ID, Image: p})
			}(r)
		}
	}
}

func handleCommand(ctx context.Context, store *Store, lst *Listener, cfg Config, keys Keys, c Command, drainNow chan<- struct{}) {
	switch c.Cmd {
	case "send":
		if strings.TrimSpace(c.Text) == "" {
			return
		}
		// Build + echo synchronously (microseconds), then hand off to
		// the outbox for the slow publish. The user sees their message
		// the instant they hit enter; the ack mark follows when the
		// bot's reaction lands.
		out, err := lst.Prepare(ctx, cfg.PeerPubKey, c.Text, c.ReplyTo)
		if err != nil {
			slog.Error("prepare", "err", err)
			pushIPC(Event{Kind: "error", Text: "prepare: " + err.Error()})
			return
		}
		m := Message{
			ID: out.Rumor.ID, PubKey: out.Rumor.PubKey, Content: out.Rumor.Content,
			TS: out.Rumor.TS, Dir: "out", Read: true, ReplyTo: c.ReplyTo,
		}
		if ok, _ := store.InsertMessage(ctx, m); ok {
			pushIPC(Event{Kind: "msg", Msg: &m})
		}
		if _, err := store.Enqueue(ctx, c.Text, c.ReplyTo); err != nil {
			slog.Error("enqueue", "err", err)
			return
		}
		select {
		case drainNow <- struct{}{}:
		default:
		}

	case "send-file":
		if c.Path == "" {
			return
		}
		// Upload + publish in a goroutine — encrypt is fast but the
		// PUT to Blossom can take seconds. No outbox persistence for
		// files: re-uploading on retry would waste bandwidth, and the
		// self-copy round-trip gives us the row anyway. If the daemon
		// dies mid-upload the user just re-attaches.
		go func(path string) {
			enc, err := encryptFile(path)
			if err != nil {
				pushIPC(Event{Kind: "error", Text: "encrypt: " + err.Error()})
				return
			}
			uctx, cancel := context.WithTimeout(ctx, 60*time.Second)
			defer cancel()
			url, err := blossomUpload(uctx, cfg.Blossom, enc, keys)
			if err != nil {
				pushIPC(Event{Kind: "error", Text: "upload: " + err.Error()})
				return
			}
			out, err := lst.PrepareFile(uctx, cfg.PeerPubKey, url, enc)
			if err != nil {
				pushIPC(Event{Kind: "error", Text: "prepare file: " + err.Error()})
				return
			}
			// Echo locally with the source path as the image — no need
			// to round-trip through Blossom to see what we just sent.
			m := Message{
				ID: out.Rumor.ID, PubKey: out.Rumor.PubKey,
				Content: "📎 " + enc.Mime, TS: out.Rumor.TS,
				Dir: "out", Read: true, Image: path,
			}
			if ok, _ := store.InsertMessage(uctx, m); ok {
				pushIPC(Event{Kind: "msg", Msg: &m})
			}
			if err := lst.Publish(uctx, out); err != nil {
				pushIPC(Event{Kind: "error", Text: "publish file: " + err.Error()})
			}
		}(c.Path)

	case "replay":
		n := c.N
		if n <= 0 {
			n = 50
		}
		msgs, err := store.Recent(ctx, n)
		if err != nil {
			slog.Warn("recent", "err", err)
			return
		}
		unread, _ := store.UnreadCount(ctx)
		pushIPC(Event{Kind: "status", Streaming: true, PubKey: keys.PK.Hex(), Name: cfg.Name, Unread: unread})
		for _, m := range msgs {
			m := m
			pushIPC(Event{Kind: "msg", Msg: &m})
		}

	case "mark-read":
		if err := store.MarkAllRead(ctx); err != nil {
			slog.Warn("mark-read", "err", err)
		}

	default:
		slog.Warn("unknown cmd", "cmd", c.Cmd)
	}
}

// publishLoop drains the outbox on its own goroutine. Re-Preparing on
// retry means a fresh rumor id each attempt — the same text can appear
// twice if a publish half-succeeded. Acceptable: persisting gift-wraps
// would drag ephemeral crypto state into sqlite.
func publishLoop(ctx context.Context, store *Store, lst *Listener, cfg Config, kick <-chan struct{}) {
	tick := time.NewTicker(30 * time.Second)
	defer tick.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-kick:
		case <-tick.C:
		}
		items, err := store.PendingOutbox(ctx, time.Now().Unix())
		if err != nil {
			slog.Warn("outbox scan", "err", err)
			continue
		}
		for _, it := range items {
			out, err := lst.Prepare(ctx, cfg.PeerPubKey, it.Content, it.ReplyTo)
			if err == nil {
				err = lst.Publish(ctx, out)
			}
			if err != nil {
				delay := time.Duration(1<<min(it.Tries, 8)) * time.Second
				_ = store.OutboxRetry(ctx, it.ID, time.Now().Add(delay).Unix())
				slog.Warn("publish failed, will retry", "tries", it.Tries+1, "delay", delay, "err", err)
				pushIPC(Event{Kind: "error", Text: fmt.Sprintf("send: %v (retrying)", err)})
				continue
			}
			_ = store.OutboxDone(ctx, it.ID)
		}
	}
}


