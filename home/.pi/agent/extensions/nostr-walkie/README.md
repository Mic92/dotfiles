# nostr-walkie

Bridge pi sessions to Nostr DMs — push agent responses to your Nostr client and
steer pi from any Nostr-capable device.

Port of
[walkie](https://github.com/aldoborrero/agent-kit/tree/main/pi/extensions/walkie)
(Telegram) to **NIP-17** private direct messages + **Blossom** file transfer.

## What it does

**pi → Nostr**

- After each agent run, the last assistant response + stats footer
  (`— 3 turns · 2 files · 4.1s`) is sent as a gift-wrapped DM.
- `<nostr-file>/path/to/file</nostr-file>` tags in the reply are stripped and
  the referenced files are encrypted + uploaded to Blossom, then sent as kind-15
  file messages.
- Session start/end notifications.
- 👀 reaction on receive, ✅ reaction on completion (NIP-25).

**Nostr → pi**

- DMs from the configured recipient pubkey become user prompts.
- 3-second debounce merges rapid consecutive messages into one prompt.
- `followUp` delivery when the agent is already busy.
- Kind-15 file messages are downloaded + decrypted; images are injected as
  attachments, other files as path references.
- `/abort` — steer + `ctx.abort()`.
- `/status` — reply with idle/busy + cwd.

**Reliability**

- Outbound events go through an in-memory publish queue with a per-relay circuit
  breaker (3 failures → 30s backoff, doubling to 30min).
- Per-event reject counter drops poison events from individual relays without
  tripping the shared breaker.
- Two-tier TTL: 24h for undelivered (warn+drop), 10min best-effort after first
  accept. Warns if nothing lands within 30s.
- Subscription auto-reconnects on drop.

**Not ported** (no Nostr equivalent): live draft streaming, typing indicator,
inline keyboards, forum-topic routing.

## Setup

Inside pi:

```
/nostr-walkie setup
```

You'll be prompted to:

1. Choose a private-key source: **g**enerate, **p**aste nsec, or **c**ommand
   (e.g. `rbw get nostr-pi`, `pass show nostr/pi`)
2. Enter **your** `npub` — only DMs from this key are accepted
3. Enter relay URLs (defaults to damus/nos.lol/primal)
4. Enter Blossom server URLs (optional — empty disables file transfer)

Config is written to `~/.pi/nostr-walkie.json`:

```json
{
  "privateKey": { "type": "command", "value": "rbw get nostr-pi" },
  "recipientPubkey": "npub1…",
  "relays": ["wss://relay.damus.io", "wss://nos.lol"],
  "blossomServers": ["https://blossom.primal.net"],
  "enabled": true
}
```

A bare-string `privateKey` is treated as `{ "type": "literal", … }` for backward
compat. A project-local `.pi/nostr-walkie.json` overrides the global config
per-key.

## Commands

| Command                    | Effect                         |
| -------------------------- | ------------------------------ |
| `/nostr-walkie`            | Show status + pi's npub        |
| `/nostr-walkie setup`      | Interactive config wizard      |
| `/nostr-walkie on` / `off` | Toggle without deleting config |

## Sending files from the agent

The model can attach files to its reply by emitting:

```
<nostr-file>/tmp/screenshot.png</nostr-file>
```

The tag is stripped from the DM text and each path is AES-256-GCM encrypted,
uploaded to Blossom, and sent as a kind-15 file message. Plaintext never leaves
the machine; key+nonce travel inside the encrypted rumor.

## Dependencies

Needs `nostr-tools` in `~/.pi/agent/node_modules/` (`ws` ships with pi). See
`../package.json`.

## Testing

1. Run `/nostr-walkie setup`, generate a key, copy the printed `npub`.
2. In Amethyst/Damus/Primal, add that npub as a contact and send it a DM.
3. The DM appears as a prompt in pi; the agent's reply comes back as a DM.

## Security notes

- Only kind-14/15 rumors whose inner `pubkey` matches `recipientPubkey` are
  accepted — gift-wraps from strangers are dropped.
- Rumors older than session start are ignored so restarting pi doesn't replay
  yesterday's backlog into the agent.
- Private key can be fetched on demand via `rbw`/`pass`/etc. instead of living
  plaintext in the JSON — use `{ "type": "command", … }`.
- Blossom uploads are ciphertext only; the server never sees plaintext.
