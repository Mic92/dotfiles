# kagi-search

CLI tool for searching Kagi using session tokens. Scrapes the HTML interface to
avoid using API credits.

## Configuration

Create `~/.config/kagi/config.json`:

```json
{
  "password_command": "rbw get kagi-session-link",
  "timeout": 30,
  "max_retries": 5
}
```

## Usage

```bash
# Search using password manager
kagi-search "search query"

# With explicit token
kagi-search -t "TOKEN" "search query"

# JSON output
kagi-search -j "search query" | jq '.[0].url'

# Limit results
kagi-search -n 5 "search query"
```

## Getting Your Token

1. Log in to [Kagi](https://kagi.com)
2. Go to [Settings → Session Link](https://kagi.com/settings?p=api)
3. Generate and copy session link
4. Store in password manager
