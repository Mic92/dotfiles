# pim - Personal Information Manager

A sandboxed AI assistant for calendar, email, contacts, and travel planning.

## Features

- **Sandboxed** (Linux): Runs in bwrap with restricted filesystem access
- **Focused toolset**: Only PIM-related tools available
- **Model**: claude-haiku-4-5

## Available Tools

| Category | Tools                                                         |
| -------- | ------------------------------------------------------------- |
| Calendar | `calendar-cli`, `vdirsyncer`, `todo`                          |
| Email    | `notmuch`, `afew`, `mrefile`, `msmtp`, `mbsync`, `email-sync` |
| Contacts | `khard`                                                       |
| Travel   | `db-cli` (German trains)                                      |
| Search   | `kagi-search`                                                 |

## Usage

```bash
pim                           # Interactive mode
pim "what's on my calendar?"  # With initial prompt
pim -c                        # Continue previous session
```

## Examples

```bash
# Calendar
pim "list my events for tomorrow"
pim "create a meeting tomorrow at 3pm for 1 hour"

# Email  
pim "search for emails from Jonas"
pim "show unread emails"

# Contacts
pim "find Jonas in my contacts"

# Travel
pim "train from Munich to Berlin tomorrow at 9am"
```
