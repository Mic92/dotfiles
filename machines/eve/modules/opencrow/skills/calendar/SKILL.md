---
name: calendar
description: Manage calendar events and todos using khal, vdirsyncer, and todoman.
---

# Calendar Management

**NEVER edit ICS files directly** — always use khal/todoman. Direct edits break
etag sync.

Run `vdirsyncer sync` before reading and after writing.

## Events (khal)

```bash
# List
khal list                          # today
khal list today 7d                 # next 7 days

# List with UIDs (needed for delete/edit)
khal list --format "{start-date} {start-time} {end-time} | {title} | {location} | {description} | {calendar} | {repeat-symbol} | [{uid}]"

# Create
khal new <date> <start> <end> "<title>" -a <calendar> [-l "<location>"] [-m "15m"] [-r weekly] [:: "<description>"]

# Delete (by UID)
printf "D\ny\ny\ny\ny\n" | khal edit <uid>

# Edit = delete old + create new
```

Options: `-m "1h,15m"` (alarms), `-r daily|weekly|monthly` (recurrence).

## Todos (todoman)

```bash
todo list
todo new "<task>"
todo done <id>
```

## Sync (vdirsyncer)

```bash
vdirsyncer sync          # sync all calendars, todos, contacts
```
