# nostr-chat

A noctalia-shell chat panel for any Nostr DM peer. Originally built for
[OpenCrow] — hit `Mod+G`, ask it to plot your Prometheus metrics, get a chart
back inline — but point it at any bot or human with a pubkey.

[OpenCrow]: https://github.com/pinpox/opencrow

![chat panel](./screenshot.png)

## What you get

- **One keystroke away** — `Mod+G` from anywhere, or let it open itself when the
  bot replies
- **Snappy** — messages appear as you type, read receipts trickle in after
- **Images** — attach via 📎, bot-generated charts render inline
- **Nothing gets lost** — history survives restarts, sends retry if the network
  is down

## Install

The plugin ships as two halves that version together:

```
nostr-chat/
├── Main.qml, Panel.qml, …   ← noctalia plugin (UI)
├── daemon/                  ← Go bridge (Nostr + sqlite)
└── module.nix               ← NixOS module: builds daemon + systemd user unit
```

### 1. Link the plugin

This directory needs to be at `~/.config/noctalia/plugins/nostr-chat`. If you're
using homesick/homeshick it's already symlinked; otherwise:

```bash
ln -s /path/to/this/dir ~/.config/noctalia/plugins/nostr-chat
```

### 2. Run the daemon

**NixOS** — import the co-located module and set your peer:

```nix
# configuration.nix
imports = [ .../nostr-chat/module.nix ];
services.nostr-chat = {
  peerPubkey  = "96dc8a8c…bf7e37";
  relays      = [ "wss://nostr.thalheim.io" "wss://nos.lol" ];
  blossom     = [ "https://nostr-files.thalheim.io" ];  # optional, for images
  displayName = "Janet";
};
```

Then `nixos-rebuild switch`. The daemon runs as a systemd _user_ unit (it needs
your rbw vault + noctalia session), so it starts with your graphical login.

**Manually:**

```bash
nix build ./daemon
NOSTR_CHAT_PEER_PUBKEY=96dc8a8c… \
NOSTR_CHAT_RELAYS=wss://nostr.thalheim.io,wss://nos.lol \
NOSTR_CHAT_BLOSSOM=https://nostr-files.thalheim.io \
NOSTR_CHAT_SECRET_CMD="rbw get 'nostr identity'" \
  ./result/bin/nostr-chatd
```

### 3. Secrets

The daemon runs whatever shell command you give it to fetch your nsec:

```nix
secretCommand = "rbw get 'nostr identity'";
# or: "pass show nostr/identity"
# or: "cat /run/agenix/nostr-nsec"
```

Add the tool to `extraPath` and any ordering deps (e.g.
`systemd.user.services.nostr-chatd.after = [ "rbw-agent.service" ]`) alongside.

## Configuration

**Daemon** (who to talk to) — NixOS options under `services.nostr-chat`:

| Option          | Purpose                                                   |
| --------------- | --------------------------------------------------------- |
| `peerPubkey`    | hex pubkey of the peer you're DMing                       |
| `relays`        | wss:// URLs both sides listen on                          |
| `blossom`       | Blossom servers for image upload (empty = no attachments) |
| `secretCommand` | shell command that prints your nsec to stdout             |
| `extraPath`     | packages on PATH (noctalia-shell + your secret tool)      |

| `displayName` | panel header label (default: `Chat`) |

The noctalia plugin settings panel only has `maxHistory` (in-memory message
cap); everything else flows from the NixOS module.

State: `~/.local/state/nostr-chatd/messages.db`. Images:
`~/.cache/nostr-chatd/media/`.

## Keybind

```nix
# niri config
binds."Mod+G".action = spawn [
  "noctalia-shell" "ipc" "call" "plugin:nostr-chat" "toggle"
];
```

Double-tap to toggle from any IPC trigger that calls `tap` instead of `toggle`
(useful if you're sharing a key with another action).

## Images

Click 📎, pick a file, it shows up in the chat. Ask the bot for a chart, it
renders inline when ready.

Files are end-to-end encrypted — the storage server only ever sees ciphertext.
Needs `NOSTR_CHAT_BLOSSOM` set (see Configuration).

## Hacking

```bash
# rebuild + restart daemon with /tmp state
./daemon/dev.sh

# reload QML
pkill -f noctalia-shell; noctalia-shell >/tmp/noctalia.log 2>&1 &

# poke it
noctalia-shell ipc call plugin:nostr-chat toggle
echo '{"cmd":"send","text":"hi"}' | nc -U "$XDG_RUNTIME_DIR/nostr-chatd.sock"
sqlite3 ~/.local/state/nostr-chatd/messages.db \
  'select dir,ack,substr(content,1,40) from messages order by ts desc limit 10'
```

The daemon pushes events into the shell via `noctalia-shell ipc call`; the shell
sends commands back over a unix socket at `$XDG_RUNTIME_DIR/nostr-chatd.sock`
(NDJSON, one-shot connections). Wire protocol is documented in `daemon/ipc.go`.

## How it works

A small Go daemon owns the Nostr connection and a sqlite history. The QML panel
is a thin view: it sends commands over a unix socket and receives events via
`noctalia-shell ipc call`. Either side can restart without losing anything — the
daemon replays from sqlite, the shell asks for a backfill on load.

Protocol details and design rationale live in the `daemon/*.go` comments.
