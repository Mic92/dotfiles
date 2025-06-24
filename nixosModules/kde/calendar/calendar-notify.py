#!/usr/bin/env python3
import dbm
import logging
import subprocess
import sys
from datetime import UTC, date, datetime, timedelta
from pathlib import Path
from typing import Any, TypeAlias

import pytz  # type: ignore[import-untyped]
from dateutil.rrule import rrulestr  # type: ignore[import-untyped]
from icalendar import Calendar, Event  # type: ignore[import-untyped]
from icalendar.prop import vDDDTypes, vDuration  # type: ignore[import-untyped]

STATE_DIR = Path.home() / ".local/state/calendar-notify"
STATE_FILE = STATE_DIR / "notified-events.db"
CALENDAR_DIR = Path.home() / ".local/share/calendars"

# Type aliases for clarity
DateOrDateTime: TypeAlias = date | datetime
DurationLike: TypeAlias = timedelta | vDuration
TriggerType: TypeAlias = datetime | timedelta | vDDDTypes
RRuleType: TypeAlias = "rrulestr | None"

# Create logger
logger = logging.getLogger(__name__)


def parse_duration(duration: DurationLike) -> timedelta:
    """Parse duration from icalendar vDuration to timedelta."""
    if hasattr(duration, "td"):
        td_value: timedelta = duration.td
        return td_value
    if isinstance(duration, timedelta):
        return duration
    # Default to 15 minutes if we can't parse
    return timedelta(minutes=15)


def parse_trigger_time(trigger: TriggerType, event_start: datetime) -> datetime | None:
    """Parse alarm trigger to get alarm time."""
    alarm_time: datetime | None = None

    # Handle different trigger types
    if hasattr(trigger, "dt"):
        if isinstance(trigger.dt, datetime):
            # Absolute datetime
            alarm_time = trigger.dt
            if alarm_time.tzinfo is None:
                alarm_time = pytz.UTC.localize(alarm_time)
        elif isinstance(trigger.dt, timedelta):
            # Relative timedelta in dt attribute (vDDDTypes)
            alarm_time = event_start + trigger.dt
    elif hasattr(trigger, "td"):
        # Relative timedelta
        td_value: timedelta = trigger.td
        alarm_time = event_start + td_value
    elif isinstance(trigger, timedelta):
        # Direct timedelta
        alarm_time = event_start + trigger
    else:
        # For vDuration or other types, the trigger itself might be the timedelta
        try:
            # If trigger is a vDuration, it might have a td attribute or be convertible
            if hasattr(trigger, "td"):
                td_value = trigger.td
                alarm_time = event_start + td_value
            # Try to use it directly as a timedelta
            elif isinstance(trigger, timedelta):
                alarm_time = event_start + trigger
        except TypeError:
            # If all else fails, use default
            alarm_time = event_start - timedelta(minutes=15)

    return alarm_time


def get_alarm_times(event: Event, event_start: datetime) -> list[datetime]:
    """Extract alarm times from a VEVENT component."""
    alarm_times: list[datetime] = []

    for component in event.walk():
        if component.name == "VALARM":
            trigger = component.get("TRIGGER")
            if trigger is not None:
                alarm_time = parse_trigger_time(trigger, event_start)
                if alarm_time is not None and isinstance(alarm_time, datetime):
                    alarm_times.append(alarm_time)

    return alarm_times


def parse_recurrence_rule(rrule_str: str, dtstart: DateOrDateTime) -> RRuleType:
    """Parse RRULE string safely."""
    try:
        # Replace RRULE: prefix if present
        rrule_str = rrule_str.removeprefix("RRULE:")
        return rrulestr(rrule_str, dtstart=dtstart)
    except (ValueError, TypeError) as e:
        # Log parsing errors
        print(f"Error parsing RRULE: {e}", file=sys.stderr)
        return None


def normalize_datetime(dt: DateOrDateTime) -> datetime:
    """Ensure datetime has timezone information."""
    if isinstance(dt, datetime):
        if dt.tzinfo is None:
            localized: datetime = pytz.UTC.localize(dt)
            return localized
        return dt
    # Handle date objects (all-day events)
    event_start = datetime.combine(dt, datetime.min.time())
    localized_date: datetime = pytz.UTC.localize(event_start)
    return localized_date


def get_event_occurrences(
    event: Event, start_date: datetime, end_date: datetime, ics_path: Path
) -> list[tuple[datetime, timedelta]]:
    """Get all occurrences of an event within a date range."""
    occurrences: list[tuple[datetime, timedelta]] = []

    # Get event start time
    dtstart = event.get("DTSTART")
    if not dtstart:
        return []

    dtstart_dt: DateOrDateTime = dtstart.dt if hasattr(dtstart, "dt") else dtstart

    # Get event end time or duration
    dtend = event.get("DTEND")
    duration = event.get("DURATION")

    if dtend:
        dtend_dt: DateOrDateTime = dtend.dt if hasattr(dtend, "dt") else dtend
        if isinstance(dtstart_dt, datetime) and isinstance(dtend_dt, datetime):
            duration = dtend_dt - dtstart_dt
        else:
            duration = timedelta(days=1)
    elif duration:
        duration = parse_duration(duration)
    else:
        duration = timedelta(hours=1)

    # Check for recurrence rules
    rrule = event.get("RRULE")
    if rrule:
        try:
            # rrule might be a vRecur object, convert it properly
            if hasattr(rrule, "to_ical"):
                rrule_str = rrule.to_ical().decode("utf-8")
            else:
                rrule_str = str(rrule)
            rule = parse_recurrence_rule(rrule_str, dtstart_dt)
            if rule:
                occurrences.extend(
                    (occurrence, duration)
                    for occurrence in rule.between(start_date, end_date, inc=True)
                )
        except (ValueError, TypeError) as e:
            # Log the specific event that failed
            summary = str(event.get("SUMMARY", "Unknown"))
            logger.warning(
                f"Error parsing RRULE for '{summary}' in {ics_path.name}: {e}. RRULE: {rrule}"
            )
            # Fall back to single occurrence
            if (
                isinstance(dtstart_dt, datetime)
                and start_date <= dtstart_dt <= end_date
            ):
                occurrences.append((dtstart_dt, duration))
            elif isinstance(dtstart_dt, date):
                # Convert date to datetime for comparison
                dt_as_datetime = normalize_datetime(dtstart_dt)
                if start_date <= dt_as_datetime <= end_date:
                    occurrences.append((dt_as_datetime, duration))
    # Single occurrence
    elif isinstance(dtstart_dt, datetime):
        if start_date <= dtstart_dt <= end_date:
            occurrences.append((dtstart_dt, duration))
    else:
        # For date objects, create datetime at midnight
        dt_as_datetime = datetime.combine(dtstart_dt, datetime.min.time())
        dt_as_datetime = (
            pytz.UTC.localize(dt_as_datetime)
            if dt_as_datetime.tzinfo is None
            else dt_as_datetime
        )
        if start_date <= dt_as_datetime <= end_date:
            occurrences.append((dt_as_datetime, duration))

    return occurrences


def should_notify(alarm_time: datetime) -> bool:
    """Check if we should notify for this alarm time."""
    now = datetime.now(UTC)
    # Notify if alarm time is in the past or within the next 5 minutes
    return alarm_time <= now + timedelta(minutes=5)


def was_notified(db: Any, event_id: str) -> bool:
    """Check if an event was already notified.

    Since event_id includes the occurrence timestamp, each occurrence
    of a recurring event has a unique ID. We just need to check if
    this specific occurrence was already notified.
    """
    key = event_id.encode("utf-8")
    return key in db


def save_notified_event(db: Any, event_id: str) -> None:
    """Mark event as notified in the database."""
    timestamp = str(int(datetime.now(UTC).timestamp()))
    db[event_id.encode("utf-8")] = timestamp.encode("utf-8")


def send_notification(title: str, time_str: str, body: str = "") -> None:
    """Send desktop notification."""
    logger.info(f"Sending notification: '{title}' at {time_str}")
    result = subprocess.run(
        [
            "notify-send",
            "--urgency=critical",  # Critical urgency keeps notification open
            "--expire-time=0",  # Never expire (0 means persistent)
            "--app-name=Calendar",
            "--icon=office-calendar",
            f"Event at {time_str}",
            title + ("\n" + body if body else ""),
        ],
        check=False,
    )
    if result.returncode == 0:
        logger.info("Notification sent successfully")
    else:
        logger.error(f"Failed to send notification, return code: {result.returncode}")


def format_notification_time(event_start: datetime, duration: timedelta) -> str:
    """Format the time string for notification."""
    now = datetime.now(UTC)
    time_str = event_start.strftime("%H:%M")
    date_str = event_start.strftime("%Y-%m-%d")

    # Include date if not today
    if event_start.date() != now.date():
        time_str = f"{date_str} {time_str}"

    # Add duration info for multi-day events
    if duration > timedelta(days=1):
        days = duration.days
        time_str += f" ({days} day event)"

    return time_str


def process_event_alarms(
    db: Any,
    event: Event,
    occurrence_start: DateOrDateTime,
    duration: timedelta,
) -> None:
    """Process alarms for a single event occurrence."""
    # Get event details
    uid = str(event.get("UID", ""))
    summary = str(event.get("SUMMARY", "Untitled Event"))
    location = str(event.get("LOCATION", ""))

    # Normalize datetime
    event_start = normalize_datetime(occurrence_start)

    # Get alarm times for this occurrence
    alarm_times = get_alarm_times(event, event_start)

    if not alarm_times:
        # No alarms defined, use default (15 minutes before)
        alarm_times = [event_start - timedelta(minutes=15)]

    # Check each alarm
    for i, alarm_time in enumerate(alarm_times):
        # Create unique ID for this occurrence and alarm
        occurrence_id = event_start.isoformat()
        event_id = f"{uid}|{occurrence_id}|alarm{i}"

        logger.debug(
            f"Checking event '{summary}' - alarm time: {alarm_time}, event time: {event_start}"
        )

        if not was_notified(db, event_id) and should_notify(alarm_time):
            logger.info(f"Alarm triggered for event '{summary}' (UID: {uid})")
            # Format notification
            time_str = format_notification_time(event_start, duration)
            send_notification(summary, time_str, location)
            save_notified_event(db, event_id)
        elif was_notified(db, event_id):
            logger.debug(f"Already notified for event '{summary}' (UID: {uid})")
        else:
            logger.debug(f"Not time yet for event '{summary}' - alarm at {alarm_time}")


def process_calendar_file(db: Any, ics_path: Path) -> None:
    """Process a single calendar file."""
    now = datetime.now(UTC)
    start_date = now - timedelta(hours=1)  # Include recently started events
    end_date = now + timedelta(days=7)

    try:
        with ics_path.open("rb") as f:
            cal = Calendar.from_ical(f.read())

        for component in cal.walk():
            if component.name == "VEVENT":
                # Get all occurrences of this event
                occurrences = get_event_occurrences(
                    component, start_date, end_date, ics_path
                )

                for occurrence_start, duration in occurrences:
                    process_event_alarms(db, component, occurrence_start, duration)

    except (ValueError, TypeError, OSError) as e:
        print(f"Error processing {ics_path}: {e}", file=sys.stderr)


def main() -> None:
    """Main function to check for calendar events with alarms."""
    STATE_DIR.mkdir(parents=True, exist_ok=True)

    # Set up logging
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(asctime)s - %(levelname)s - %(message)s",
    )

    logger.info("Starting calendar notification check")
    logger.info(f"Current time: {datetime.now(UTC)}")

    # Open database once for the entire run
    with dbm.open(str(STATE_FILE), "c") as db:
        # Scan all ICS files in calendar directories
        for ics_path in CALENDAR_DIR.rglob("*.ics"):
            process_calendar_file(db, ics_path)

    logger.info("Calendar notification check completed")


if __name__ == "__main__":
    main()
