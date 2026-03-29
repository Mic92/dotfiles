---
name: slack
description: Read Slack — search messages, list channels/users, read history and thread replies.
---

```bash
n8n-hooks slack search "<query>" [-n 20]          # e.g. "in:#dev deploy from:@mic92"
n8n-hooks slack history <CHANNEL_ID> [-n 50]      # recent messages in a channel
n8n-hooks slack replies <CHANNEL_ID> <THREAD_TS>  # full thread
n8n-hooks slack list-channels                     # channel name → ID
n8n-hooks slack list-users                        # user name → ID
```

Channel and user arguments use **IDs** (`C0123…`, `U0123…`) — run
`list-channels` / `list-users` first to resolve names. `search` accepts Slack's
query syntax (`in:`, `from:`, `before:`, etc.) and returns the channel ID and
`ts` needed to follow up with `history` or `replies`.
