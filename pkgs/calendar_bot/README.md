# Matrix Calendar Bot

A Matrix bot that bridges encrypted Matrix rooms to n8n webhooks for calendar
automation.

## Overview

This bot listens for commands in Matrix rooms (messages starting with `!`) and
forwards them to an n8n webhook for processing. It supports full end-to-end
encryption using mautrix-python.

## Features

- **Full E2E Encryption**: Uses mautrix-python's encryption with
  PostgreSQL-backed key storage
- **Auto-trust Unverified Devices**: Configured with
  `send_keys_min_trust = UNVERIFIED` to send encryption keys to all devices in
  the room without requiring verification
- **Session Persistence**: Maintains device identity across restarts using
  PostgreSQL
- **Webhook Integration**: Forwards commands to n8n for processing and returns
  responses

## Architecture

```
Matrix Room (encrypted)
    ↓
Calendar Bot (decrypts, filters commands starting with !)
    ↓
n8n Webhook (processes calendar commands)
    ↓
Calendar Bot (encrypts and sends response)
    ↓
Matrix Room (encrypted)
```

## Storage

- **PostgreSQL Database**: `calendar_bot` - stores encryption keys, device
  trust, and sync state
- **State Directory**: `/var/lib/calendar-bot/` - contains state.json for room
  membership tracking

## Configuration

### Command Line Arguments

```bash
calendar-bot \
  --homeserver https://matrix.example.org \
  --username @calendar:example.org \
  --password-file /path/to/password \
  --webhook-url https://n8n.example.org/webhook/calendar-bot \
  --auth-token-file /path/to/webhook-token \
  --database-url postgresql:///calendar_bot \
  --store-path /var/lib/calendar-bot \
  --device-name calendar-bot \
  --log-level INFO
```

### NixOS Module Options

```nix
services.calendar-bot = {
  enable = true;
  homeserver = "https://matrix.example.org";
  username = "@calendar:example.org";
  webhookUrl = "https://n8n.example.org/webhook/calendar-bot";
  passwordFile = "/path/to/password";
  authTokenFile = "/path/to/auth-token";  # optional
};
```

## Encryption & Trust

The bot uses mautrix-python's default trust levels:

- **Sending keys**: `TrustState.UNVERIFIED` - Sends encryption keys to ALL
  devices in the room
- **Receiving keys**: `TrustState.CROSS_SIGNED_TOFU` - Only accepts keys from
  cross-signed devices (Trust On First Use)

This ensures:

- All users can decrypt the bot's messages (no "Failed to decrypt" errors)
- The bot only trusts properly configured devices
- No manual device verification required

## Deployment

### Initial Setup

1. **Database Creation**: Automatically handled by `clan.core.postgresql` module
2. **First Run**: Bot logs in with password, creates Olm account, stores session
   in PostgreSQL
3. **Subsequent Runs**: Bot uses stored session and device identity from
   database

### Manual PostgreSQL Operations

```bash
# View crypto tables
sudo -u postgres psql calendar_bot -c '\dt'

# Reset bot identity (drops all encryption keys)
sudo -u postgres dropdb calendar_bot
sudo -u postgres createdb calendar_bot -O calendar-bot \
  -E UTF8 --lc-collate=C --lc-ctype=C
systemctl restart calendar-bot
```

## Troubleshooting

### Users Can't Decrypt Bot Messages

This should not happen with mautrix-python. If it does:

1. Check bot logs: `journalctl -u calendar-bot -f`
2. Verify encryption is initialized: Look for "Encryption initialized" in logs
3. Check database connection:
   `sudo -u postgres psql calendar_bot -c 'SELECT COUNT(*) FROM crypto_device'`

### Bot Can't Decrypt User Messages

- Check if room is encrypted: `await client.state_store.is_encrypted(room_id)`
- Verify users are using cross-signing (required for key reception)
- Check key request logs

### Session Invalidated (M_UNKNOWN_TOKEN)

If the homeserver invalidates the session:

1. Bot will crash with M_UNKNOWN_TOKEN error
2. Manually clear database:
   `sudo -u postgres psql calendar_bot -c 'DELETE FROM crypto_account'`
3. Restart service: `systemctl restart calendar-bot`

### Database Corruption

```bash
# Full reset (loses all encryption keys and message history)
systemctl stop calendar-bot
sudo -u postgres dropdb calendar_bot
sudo -u postgres createdb calendar_bot -O calendar-bot \
  -E UTF8 --lc-collate=C --lc-ctype=C
systemctl start calendar-bot
```

### Dependencies

- **mautrix-python**: Matrix client library with E2E encryption
- **aiohttp**: HTTP client for webhook requests
- **asyncpg**: PostgreSQL async driver

## Security

- Password stored in systemd credentials (not in process environment)
- Webhook auth token optional but recommended
- Hardened systemd service with:
  - `PrivateTmp`, `ProtectSystem=strict`, `ProtectHome`
  - `NoNewPrivileges`, `PrivateDevices`
  - `RestrictNamespaces`, `LockPersonality`

## License

MIT
