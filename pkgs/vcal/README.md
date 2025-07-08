# vcal

vCalendar/iCalendar management tool for creating, importing, and replying to
calendar invitations.

## Standards Compliance

vcal implements the following RFC standards:

- **RFC 5545** (iCalendar): Core calendar data format specification
  - Content line folding (Section 3.1)
  - VEVENT components and properties
  - VALARM components for reminders
  - Recurrence rules (RRULE)

- **RFC 5546** (iTIP - iCalendar Transport-Independent Interoperability
  Protocol):
  - METHOD:REQUEST for sending invitations
  - METHOD:REPLY for RSVP responses
  - Proper ATTENDEE participation status handling
  - Required and optional properties compliance

## Features

- **vcal create**: Create and send calendar invitations via email
- **vcal import**: Import calendar invites from emails or .ics files into your
  local calendar
- **vcal reply**: Send RSVP responses (accept/decline/tentative) to calendar
  invitations

## Installation

```bash
pip install -e .
```

## Usage

### Creating Calendar Invites

```bash
# Simple meeting
vcal create -s "Team Meeting" -d 60 -a "john@example.com,jane@example.com"

# With Zoom link
vcal create -s "Project Review" -d 90 -a "team@example.com" \
  -l "https://zoom.us/j/123456789" --meeting-link "https://zoom.us/j/123456789"

# Recurring weekly meeting
vcal create -s "Weekly Standup" -d 30 -a "team@example.com" \
  --repeat weekly --count 10
```

### Importing Calendar Invites

```bash
# Import from email (piped from mail client)
cat email.eml | vcal import

# Import from .ics file
vcal import meeting.ics

# Import to specific calendar
vcal import -c Work meeting.ics
```

### Replying to Calendar Invites

```bash
# Reply to invite from email
cat email.eml | vcal reply accept
cat email.eml | vcal reply decline
cat email.eml | vcal reply tentative

# With comment
cat email.eml | vcal reply decline -c "Sorry, I have another meeting"
```

## Integration with aerc

Add to your aerc binds.conf:

```ini
# Calendar invite commands
ii = :pipe -m vcal import<Enter>
ia = :pipe -m vcal reply accept<Enter>
id = :pipe -m vcal reply decline<Enter>
it = :pipe -m vcal reply tentative<Enter>
```

## Requirements

- Python 3.13+
- khal (for calendar import)
- vdirsyncer (for calendar sync)
- msmtp (for sending emails)
