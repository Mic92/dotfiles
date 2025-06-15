# Khal Calendar Configuration

This directory contains the configuration for khal, a CLI calendar application
that syncs with your Nextcloud calendars via vdirsyncer.

## Overview

- **Calendars synced**:
  - `personal` - Your personal calendar
  - `joergshannan` - Shared calendar with Shannan

- **Sync frequency**: Every 15 minutes via systemd timer
- **Notifications**: Every 5 minutes for events in the next hour

## Usage

### View calendars

```bash
# List events for today
khal list

# List events for next 7 days
khal list now 7d

# View calendar interface
khal calendar

# Search for events
khal search "meeting"
```

### Create events

```bash
# Quick event
khal new "16:00" "Coffee with Bob"

# Event with duration
khal new "16:00" 1h "Team meeting"

# Event with description
khal new "16:00" 1h "Team meeting" :: "Discuss Q3 goals"

# All-day event
khal new 2025-06-20 "Vacation day"

# Event in specific calendar (joergshannan)
khal new -a joergshannan1 "18:00" 2h "Dinner with Shannan" :: "Restaurant reservation at Italian place"

# Event on specific date and time
khal new 2025-06-25 "14:30" 1h "Doctor appointment" :: "Annual checkup"
```

### Sync manually

```bash
# Sync all calendars
vdirsyncer sync

# Check sync status
systemctl --user status calendar-sync.timer
```

## Systemd Services

- `calendar-sync.service` - Syncs calendars with vdirsyncer
- `calendar-sync.timer` - Runs sync every 15 minutes
- `calendar-notify.service` - Checks for upcoming events and sends notifications
- `calendar-notify.timer` - Runs notification check every 5 minutes

## Troubleshooting

```bash
# Check service logs
journalctl --user -u calendar-sync
journalctl --user -u calendar-notify

# List timers
systemctl --user list-timers | grep calendar

# Restart services
systemctl --user restart calendar-sync.timer
systemctl --user restart calendar-notify.timer
```

## Configuration Files

- `config` - Khal configuration
- `~/.config/vdirsyncer/config` - Vdirsyncer configuration
- `~/.local/share/calendars/` - Local calendar storage
- `~/.local/state/calendar-notify/` - Notification state tracking
