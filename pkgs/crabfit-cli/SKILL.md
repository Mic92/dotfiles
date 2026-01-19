---
name: crabfit-cli
description: Create and manage Crab.fit scheduling events. Use for coordinating meeting times across multiple people.
---

# Usage

```bash
# Create an event for the next 3 days, 9am-5pm
python3 crabfit_cli.py create --dates +0:+2 --start 9 --end 17

# Create a named event for specific dates
python3 crabfit_cli.py create --name "Team Meeting" --dates 2026-01-20,2026-01-22

# Create with specific timezone
python3 crabfit_cli.py create --dates +0:+6 --timezone America/New_York

# Add your availability (all slots)
python3 crabfit_cli.py respond EVENT_ID --name "Alice" --all

# Add specific availability
python3 crabfit_cli.py respond EVENT_ID --name "Bob" --slots 1000-19012026 1100-19012026

# Show event with availability overlap
python3 crabfit_cli.py show EVENT_ID
```

# Date Formats

- Single date: `2026-01-20`
- Range: `2026-01-20:2026-01-25`
- Comma-separated: `2026-01-20,2026-01-22,2026-01-24`
- Relative: `+0` (today), `+1` (tomorrow), `+0:+6` (next 7 days)

# Time Slot Format

Slots use `HHmm-DDMMYYYY` format in UTC (e.g., `0900-19012026` = 09:00 UTC on
Jan 19, 2026).

# Environment Variables

- `CRABFIT_API_URL`: API base URL (default: `https://api.crab.fit`)

# Example Workflow

```bash
# 1. Create event
python3 crabfit_cli.py create --name "Project Sync" --dates +1:+5 --start 10 --end 16

# 2. Share URL with participants, they respond via web or CLI

# 3. Check availability overlap
python3 crabfit_cli.py show project-sync-123456
```

Output shows best meeting times:

```
All 3 available:
  ✓ Mon Jan 19 11:00-13:00
  ✓ Tue Jan 20 14:00-16:00

2/3 available:
  • Mon Jan 19 10:00-11:00  (missing: Bob)
```
