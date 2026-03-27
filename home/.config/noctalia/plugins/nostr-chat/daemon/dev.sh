#!/usr/bin/env bash
# Dev loop: restart the daemon against the live shell.
# The plugin dir is a homesick symlink into this repo, so both QML
# edits and daemon rebuilds are live — no deploy step.
set -euo pipefail
cd "$(dirname "$0")"

# Stop any systemd-managed or prior dev instance so we don't fight
# over the socket. `go run` spawns the real process as exe/daemon
# under go-build's cache, hence the second pattern.
systemctl --user stop nostr-chatd.service 2>/dev/null || true
pkill -f 'nostr-chatd|go-build.*/exe/daemon' 2>/dev/null || true

export NOSTR_CHAT_PEER_PUBKEY=96dc8a8cb0c28bdd113c1f6e350abd6014c69369cbd618c3b8cd4d1326bf7e37
export NOSTR_CHAT_RELAYS=wss://nostr.thalheim.io,wss://nos.lol,wss://nostr.0cx.de
export NOSTR_CHAT_SECRET_CMD="rbw get 'nostr identity'"
export NOSTR_CHAT_BLOSSOM=https://nostr-files.thalheim.io
export NOSTR_CHAT_DISPLAY_NAME=Janet
export CGO_ENABLED=0

# Uses the real ~/.local/state/nostr-chatd — dev and prod share history,
# so swapping between dev.sh and the systemd unit is seamless.
exec nix-shell -p go --run "go run . $*"
