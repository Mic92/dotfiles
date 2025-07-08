# ical-cli

Command-line tools for working with iCalendar files and email invitations.

## Features

- **create-calendar-invite**: Create and send calendar invitations via email
- **import-calendar-invite**: Import calendar invites from emails or .ics files
  into your local calendar
- **reply-calendar-invite**: Send RSVP responses (accept/decline/tentative) to
  calendar invitations

## Installation

```bash
pip install -e .
```

## Usage

### Creating Calendar Invites

```bash
# Simple meeting
create-calendar-invite -s "Team Meeting" -d 60 -a "john@example.com,jane@example.com"

# With Zoom link
create-calendar-invite -s "Project Review" -d 90 -a "team@example.com" \
  -l "https://zoom.us/j/123456789" --meeting-link "https://zoom.us/j/123456789"

# Recurring weekly meeting
create-calendar-invite -s "Weekly Standup" -d 30 -a "team@example.com" \
  --repeat weekly --count 10
```

### Importing Calendar Invites

```bash
# Import from email (piped from mail client)
cat email.eml | import-calendar-invite

# Import from .ics file
import-calendar-invite meeting.ics

# Import to specific calendar
import-calendar-invite -c Work meeting.ics
```

### Replying to Calendar Invites

```bash
# Reply to invite from email
cat email.eml | reply-calendar-invite accept
cat email.eml | reply-calendar-invite decline
cat email.eml | reply-calendar-invite tentative

# With comment
cat email.eml | reply-calendar-invite decline -c "Sorry, I have another meeting"
```

## Integration with aerc

Add to your aerc binds.conf:

```ini
# Calendar invite commands
ii = :pipe -m import-calendar-invite<Enter>
ia = :pipe -m reply-calendar-invite accept<Enter>
id = :pipe -m reply-calendar-invite decline<Enter>
it = :pipe -m reply-calendar-invite tentative<Enter>
```

## Requirements

- Python 3.9+
- khal (for calendar import)
- vdirsyncer (for calendar sync)
- msmtp (for sending emails)
