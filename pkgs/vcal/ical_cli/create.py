#!/usr/bin/env python3
"""Generate ICS calendar invites and send them via msmtp.

Supports attendees with names/emails and meeting links.
"""

from __future__ import annotations

import argparse
import os
import pwd
import re
import socket
import subprocess
import sys
import tempfile
import uuid
from dataclasses import dataclass
from datetime import datetime, timedelta
from email import encoders
from email.mime.base import MIMEBase
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from pathlib import Path

import pytz
from icalendar import Alarm, Calendar, Event, vCalAddress, vText
from pytz import timezone


@dataclass
class MeetingConfig:
    """Configuration for a meeting invitation."""

    summary: str
    start: datetime
    end: datetime
    organizer_name: str
    organizer_email: str
    attendees: list[tuple[str, str]]
    location: str | None = None
    meeting_link: str | None = None
    description: str | None = None
    reminder_minutes: int = 15
    tz: str = "UTC"
    recurrence: dict | None = None


@dataclass
class CreateConfig:
    """Configuration for create command."""

    meeting: MeetingConfig
    output: str | None = None
    no_send: bool = False
    dry_run: bool = False
    calendar_dir: str = "~/.local/share/calendars/Personal"
    no_local_save: bool = False


def parse_attendees(attendees_str: str) -> list[tuple[str, str]]:
    """Parse attendee string format: 'Name <email>' or just 'email'."""
    attendees = []
    # Handle empty string
    if not attendees_str.strip():
        return attendees

    for attendee_str in attendees_str.split(","):
        attendee = attendee_str.strip()
        if not attendee:  # Skip empty items from split
            continue
        match = re.match(r"^(.+?)\s*<(.+?)>$", attendee)
        if match:
            name, email = match.groups()
            attendees.append((name.strip(), email.strip()))
        elif "@" in attendee:
            # Just an email address
            attendees.append(("", attendee))
        else:
            msg = f"Invalid attendee format: {attendee}"
            raise ValueError(msg)
    return attendees


def create_calendar_invite(config: MeetingConfig) -> Calendar:
    """Create an ICS calendar invite."""
    cal = Calendar()
    cal.add("prodid", "-//Calendar Invite Generator//mxm.dk//")
    cal.add("version", "2.0")
    cal.add("method", "REQUEST")

    event = Event()

    # Basic event properties
    event.add("summary", config.summary)
    event.add("dtstart", config.start)
    event.add("dtend", config.end)
    event.add("dtstamp", datetime.now(pytz.utc))
    event.add("uid", f"{uuid.uuid4()}@calendar-invite-generator")
    event.add("sequence", 0)
    event.add("status", "CONFIRMED")
    event.add("transp", "OPAQUE")

    # Location and meeting link
    if config.location:
        event.add("location", config.location)

    # Description with meeting link
    desc_parts = []
    if config.description:
        desc_parts.append(config.description)
    if config.meeting_link:
        desc_parts.append(f"\nJoin meeting: {config.meeting_link}")
    if desc_parts:
        event.add("description", "\n".join(desc_parts))

    # Organizer
    organizer = vCalAddress(f"mailto:{config.organizer_email}")
    organizer.params["cn"] = vText(config.organizer_name)
    organizer.params["role"] = vText("CHAIR")
    event.add("organizer", organizer)

    # Add organizer as attendee (so it appears in their calendar)
    org_attendee = vCalAddress(f"mailto:{config.organizer_email}")
    org_attendee.params["cn"] = vText(config.organizer_name)
    org_attendee.params["partstat"] = vText("ACCEPTED")
    org_attendee.params["role"] = vText("REQ-PARTICIPANT")
    event.add("attendee", org_attendee, encode=0)

    # Attendees
    for name, email in config.attendees:
        attendee = vCalAddress(f"mailto:{email}")
        if name:
            attendee.params["cn"] = vText(name)
        attendee.params["partstat"] = vText("NEEDS-ACTION")
        attendee.params["rsvp"] = vText("TRUE")
        attendee.params["role"] = vText("REQ-PARTICIPANT")
        event.add("attendee", attendee, encode=0)

    # Add reminder/alarm
    alarm = Alarm()
    alarm.add("action", "DISPLAY")
    alarm.add("description", f"Reminder: {config.summary}")
    alarm.add("trigger", timedelta(minutes=-config.reminder_minutes))
    event.add_component(alarm)

    # Add recurrence rule if specified
    if config.recurrence:
        event.add("rrule", config.recurrence)

    cal.add_component(event)
    return cal


@dataclass
class EmailConfig:
    """Configuration for sending email."""

    cal: Calendar
    config: MeetingConfig
    dry_run: bool = False


def send_invite_email(email_config: EmailConfig) -> bool:
    """Send calendar invite via msmtp."""
    # Create email message
    msg = MIMEMultipart("mixed")
    msg["Subject"] = f"Invitation: {email_config.config.summary}"
    msg["From"] = f"{email_config.config.organizer_name} <{email_config.config.organizer_email}>"

    # Build recipient list
    recipients = []
    to_list = []
    for name, email in email_config.config.attendees:
        recipients.append(email)
        if name:
            to_list.append(f"{name} <{email}>")
        else:
            to_list.append(email)

    msg["To"] = ", ".join(to_list)

    # Create email body
    body_text = f"""You have been invited to: {email_config.config.summary}

When: {email_config.config.start.strftime("%A, %B %d, %Y")}
Time: {email_config.config.start.strftime("%I:%M %p")} - {email_config.config.end.strftime("%I:%M %p %Z")}"""  # noqa: E501

    if email_config.config.meeting_link:
        body_text += f"\n\nJoin meeting: {email_config.config.meeting_link}"

    body_text += "\n\nPlease see the attached calendar invitation for details."

    # Add text part
    msg.attach(MIMEText(body_text, "plain"))

    # Add calendar part
    cal_part = MIMEBase("text", "calendar")
    cal_part.set_payload(email_config.cal.to_ical())
    cal_part.add_header("Content-Disposition", 'attachment; filename="invite.ics"')
    cal_part.add_header(
        "Content-Type",
        'text/calendar; charset="UTF-8"; method=REQUEST',
    )
    encoders.encode_base64(cal_part)
    msg.attach(cal_part)

    # Send email
    if email_config.dry_run:
        return True

    # Use msmtp to send
    try:
        # Write message to temporary file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".eml", delete=False) as f:
            f.write(msg.as_string())
            temp_file = f.name

        # Send with msmtp
        cmd = ["msmtp", "-t"]
        with Path(temp_file).open() as f:
            result = subprocess.run(
                cmd,
                check=False,
                stdin=f,
                capture_output=True,
                text=True,
            )

        # Clean up temp file
        Path(temp_file).unlink()

        if result.returncode != 0:
            return False

    except OSError:
        return False
    else:
        return True


def get_local_timezone() -> str:
    """Get the system's local timezone."""
    try:
        # Try reading from /etc/timezone first (common on many Linux distros)
        timezone_file = Path("/etc/timezone")
        if timezone_file.exists():
            return timezone_file.read_text().strip()
    except OSError:
        pass

    try:
        # Try reading the symlink target of /etc/localtime
        localtime_path = Path("/etc/localtime")
        if localtime_path.is_symlink():
            # Extract timezone from path like /usr/share/zoneinfo/Europe/Berlin
            tz_path = str(localtime_path.readlink())
            if "zoneinfo/" in tz_path:
                return tz_path.split("zoneinfo/", 1)[1]
    except OSError:
        pass

    # Check TZ environment variable
    tz_env = os.environ.get("TZ")
    if tz_env:
        return tz_env

    # Default to UTC if we can't determine local timezone
    return "UTC"




def get_organizer_info(args: argparse.Namespace) -> tuple[str, str]:
    """Get organizer name and email from args or system."""
    organizer_name = args.organizer_name
    if not organizer_name:
        try:
            # Try to get the real name from passwd entry
            pw_entry = pwd.getpwuid(os.getuid())
            organizer_name = pw_entry.pw_gecos.split(",")[0]
            # Fallback to username if gecos is empty
            if not organizer_name:
                organizer_name = pw_entry.pw_name
        except (KeyError, OSError):
            # Last resort fallback
            organizer_name = "Meeting Organizer"

    organizer_email = args.organizer_email
    if not organizer_email:
        organizer_email = os.environ.get("EMAIL")
        if not organizer_email:
            try:
                # Get username without using os.getlogin()
                username = pwd.getpwuid(os.getuid()).pw_name
            except (KeyError, OSError):
                username = os.environ.get("USER", "user")
            hostname = socket.gethostname()
            organizer_email = f"{username}@{hostname}"

    return organizer_name, organizer_email


def parse_meeting_time(
    args: argparse.Namespace,
    tz: pytz.tzinfo.BaseTzInfo,
) -> tuple[datetime, datetime]:
    """Parse start time and calculate end time."""
    if args.start:
        try:
            naive_dt = datetime.strptime(args.start, "%Y-%m-%d %H:%M")  # noqa: DTZ007
            start = tz.localize(naive_dt)
        except ValueError:
            msg = "Invalid start time format. Use YYYY-MM-DD HH:MM"
            raise ValueError(msg) from None
    else:
        # Default to next hour
        now = datetime.now(tz)
        start = now.replace(minute=0, second=0, microsecond=0) + timedelta(hours=1)

    end = start + timedelta(minutes=args.duration)
    return start, end


def parse_custom_rrule(rrule_str: str) -> dict:
    """Parse custom RRULE string."""
    rrule_parts = {}
    for part in rrule_str.split(";"):
        if "=" in part:
            key, value = part.split("=", 1)
            if key == "UNTIL" and value.endswith("Z"):
                # Convert UNTIL string to datetime
                rrule_parts[key] = datetime.strptime(value, "%Y%m%dT%H%M%SZ").replace(
                    tzinfo=pytz.utc,
                )
            elif key == "BYDAY":
                # Split comma-separated days into a list
                rrule_parts[key] = [day.strip() for day in value.split(",")]
            else:
                rrule_parts[key] = value
    return rrule_parts


def parse_recurrence_options(
    args: argparse.Namespace,
    tz: pytz.tzinfo.BaseTzInfo,
) -> dict | None:
    """Parse recurrence options from command line arguments."""
    if args.rrule:
        return parse_custom_rrule(args.rrule)

    if not args.repeat:
        return None

    # Build recurrence rule from simple options
    rrule = {}

    # Set frequency
    freq_map = {
        "daily": "DAILY",
        "weekly": "WEEKLY",
        "biweekly": "WEEKLY",
        "monthly": "MONTHLY",
        "yearly": "YEARLY",
    }
    rrule["FREQ"] = freq_map[args.repeat]

    # Handle biweekly
    if args.repeat == "biweekly":
        rrule["INTERVAL"] = 2

    # Handle weekdays for weekly recurrence
    if args.repeat in ["weekly", "biweekly"] and args.weekdays:
        # Split comma-separated weekdays into a list
        rrule["BYDAY"] = [day.strip().upper() for day in args.weekdays.split(",")]

    # Handle count or until
    if args.count:
        rrule["COUNT"] = args.count
    elif args.until:
        try:
            until_date = datetime.strptime(args.until, "%Y-%m-%d")  # noqa: DTZ007
            # Convert to UTC for iCalendar - use datetime object directly
            until_dt = tz.localize(until_date.replace(hour=23, minute=59, second=59))
            rrule["UNTIL"] = until_dt.astimezone(pytz.utc)
        except ValueError:
            msg = "Invalid until date format. Use YYYY-MM-DD"
            raise ValueError(msg) from None

    return rrule


def save_ics_file(cal: Calendar, output: str | None, start: datetime) -> None:
    """Save ICS file if requested."""
    if output:
        output_file = output
    else:
        # Generate filename from summary and date
        event = cal.walk("vevent")[0]
        summary = str(event.get("summary", "meeting"))
        safe_summary = re.sub(r"[^\w\s-]", "", summary).strip().replace(" ", "-")
        date_str = start.strftime("%Y%m%d")
        output_file = f"{safe_summary}-{date_str}.ics"

    output_path = Path(output_file)
    with output_path.open("wb") as f:
        f.write(cal.to_ical())


def save_to_local_calendar(cal: Calendar, calendar_dir: str) -> None:
    """Save event to local calendar directory."""
    cal_dir = Path(calendar_dir).expanduser()
    if cal_dir.is_dir():
        # Generate unique filename using UID
        event_uid = cal.walk("vevent")[0]["UID"]
        calendar_file = cal_dir / f"{event_uid}.ics"
        try:
            with calendar_file.open("wb") as f:
                f.write(cal.to_ical())
        except OSError:
            pass  # Silently ignore save errors
    else:
        pass  # Directory not found is expected in some environments


def format_recurrence_description(recurrence: dict) -> str:
    """Format recurrence rule into human-readable description."""
    recur_desc = []
    freq = recurrence.get("FREQ", "").lower()
    if freq:
        interval = recurrence.get("INTERVAL", 1)
        biweekly_interval = 2
        if interval == biweekly_interval and freq == "weekly":
            recur_desc.append("Repeats: biweekly")
        else:
            recur_desc.append(f"Repeats: {freq}")

    if "BYDAY" in recurrence:
        days = recurrence["BYDAY"]
        if isinstance(days, list):
            recur_desc.append(f"on {','.join(days)}")
        else:
            recur_desc.append(f"on {days}")

    if "COUNT" in recurrence:
        recur_desc.append(f"for {recurrence['COUNT']} occurrences")
    elif "UNTIL" in recurrence:
        # Handle UNTIL as either datetime or string
        until = recurrence["UNTIL"]
        if isinstance(until, datetime):
            recur_desc.append(f"until {until.strftime('%Y-%m-%d')}")
        elif isinstance(until, str) and until.endswith("Z"):
            until_dt = datetime.strptime(until, "%Y%m%dT%H%M%SZ").replace(
                tzinfo=pytz.utc,
            )
            recur_desc.append(f"until {until_dt.strftime('%Y-%m-%d')}")

    return " ".join(recur_desc)


def print_meeting_details(config: MeetingConfig) -> None:
    """Print meeting details summary."""
    # Show recurrence info
    if config.recurrence:
        recur_desc = format_recurrence_description(config.recurrence)
        if recur_desc:
            print(f"Recurrence: {recur_desc}")

    print("\nAttendees:")
    for name, email in config.attendees:
        if name:
            print(f"  - {name} <{email}>")
        else:
            print(f"  - {email}")

    if config.meeting_link:
        print(f"\nMeeting link: {config.meeting_link}")


def run(config: CreateConfig) -> int:
    """Run the create command with the given configuration."""
    # Create calendar invite
    cal = create_calendar_invite(config.meeting)

    # Save ICS file if requested
    if config.output or config.no_send:
        save_ics_file(cal, config.output, config.meeting.start)

    # Print meeting details
    print_meeting_details(config.meeting)

    # Save to local calendar if not disabled
    if not config.no_local_save:
        save_to_local_calendar(cal, config.calendar_dir)

    # Send email if not disabled
    if not config.no_send:
        email_config = EmailConfig(
            cal=cal,
            config=config.meeting,
            dry_run=config.dry_run,
        )
        success = send_invite_email(email_config)

        if not success and not config.dry_run:
            return 1

    return 0


def _handle_args(args: argparse.Namespace) -> int:
    # Parse timezone
    try:
        tz = timezone(args.timezone)
    except pytz.exceptions.UnknownTimeZoneError:
        print(f"Error: Unknown timezone: {args.timezone}", file=sys.stderr)
        return 1

    # Parse time and attendees
    try:
        start, end = parse_meeting_time(args, tz)
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    attendees = parse_attendees(args.attendees)
    if not attendees:
        print("Error: No valid attendees specified", file=sys.stderr)
        return 1

    # Get organizer info
    organizer_name, organizer_email = get_organizer_info(args)

    # Parse recurrence options
    try:
        recurrence = parse_recurrence_options(args, tz)
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    # Create meeting config
    meeting = MeetingConfig(
        summary=args.summary,
        start=start,
        end=end,
        organizer_name=organizer_name,
        organizer_email=organizer_email,
        attendees=attendees,
        location=args.location,
        meeting_link=args.meeting_link,
        description=args.description,
        reminder_minutes=args.reminder,
        tz=args.timezone,
        recurrence=recurrence,
    )

    # Create full config
    config = CreateConfig(
        meeting=meeting,
        output=args.output,
        no_send=args.no_send,
        dry_run=args.dry_run,
        calendar_dir=args.calendar_dir,
        no_local_save=args.no_local_save,
    )

    return run(config)


def register_parser(subparsers: argparse._SubParsersAction) -> None:
    """Register the create subcommand."""
    parser = subparsers.add_parser(
        "create",
        help="Create and send calendar invitations",
        description="Generate and send ICS calendar invites via email",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Simple meeting
  vcal create -s "Team Meeting" -d 60 -a "john@example.com,Jane Doe <jane@example.com>"

  # Meeting with Zoom link
  vcal create -s "Project Review" -d 90 -a "team@example.com" \\
    -l "https://zoom.us/j/123456789" --meeting-link "https://zoom.us/j/123456789"

  # Specific time and timezone
  vcal create -s "Client Call" --start "2024-01-15 14:00" -d 30 \\
    -a "Client Name <client@company.com>" --timezone "America/New_York"

  # Weekly recurring meeting for 10 weeks
  vcal create -s "Weekly Standup" -d 30 -a "team@example.com" \\
    --repeat weekly --count 10

  # Biweekly meeting on specific days until end of year
  vcal create -s "Sprint Review" -d 60 -a "team@example.com" \\
    --repeat biweekly --weekdays "MO,FR" --until "2025-12-31"

  # Daily meeting for 5 days
  vcal create -s "Daily Sync" -d 15 -a "team@example.com" \\
    --repeat daily --count 5

  # Custom recurrence rule (every 3 days, 7 times)
  vcal create -s "Check-in" -d 30 -a "manager@example.com" \\
    --rrule "FREQ=DAILY;INTERVAL=3;COUNT=7"

  # Save ICS file without sending
  vcal create -s "Meeting" -a "test@example.com" --no-send -o meeting.ics
        """,
    )

    # Add all arguments from create_argument_parser
    local_tz = get_local_timezone()

    parser.add_argument("-s", "--summary", required=True, help="Meeting title/summary")
    parser.add_argument(
        "--start",
        help="Start time (YYYY-MM-DD HH:MM), defaults to next hour",
    )
    parser.add_argument(
        "-d",
        "--duration",
        type=int,
        default=60,
        help="Duration in minutes (default: 60)",
    )
    parser.add_argument(
        "-a",
        "--attendees",
        required=True,
        help='Comma-separated attendees: "Name <email>" or just "email"',
    )
    parser.add_argument("--organizer-name", help="Your name (defaults to system user)")
    parser.add_argument(
        "--organizer-email",
        help="Your email (defaults to $EMAIL or user@hostname)",
    )
    parser.add_argument("-l", "--location", help="Meeting location")
    parser.add_argument("--meeting-link", help="Meeting URL (Zoom, Teams, etc.)")
    parser.add_argument("--description", help="Additional meeting description")
    parser.add_argument(
        "--reminder",
        type=int,
        default=15,
        help="Reminder minutes before (default: 15)",
    )
    parser.add_argument(
        "--timezone",
        default=local_tz,
        help=f"Timezone (default: {local_tz})",
    )
    parser.add_argument(
        "-o",
        "--output",
        help="Save ICS to file (in addition to sending)",
    )
    parser.add_argument(
        "--no-send",
        action="store_true",
        help="Only create ICS file, do not send",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be sent without sending",
    )
    parser.add_argument(
        "--calendar-dir",
        default="~/.local/share/calendars/Personal",
        help=(
            "Local calendar directory for saving events "
            "(default: ~/.local/share/calendars/Personal)"
        ),
    )
    parser.add_argument(
        "--no-local-save",
        action="store_true",
        help="Do not save to local calendar",
    )

    # Recurrence options
    recurrence_group = parser.add_argument_group("recurrence options")
    recurrence_group.add_argument(
        "--repeat",
        choices=["daily", "weekly", "biweekly", "monthly", "yearly"],
        help="Simple recurrence pattern",
    )
    recurrence_group.add_argument(
        "--count",
        type=int,
        help="Number of occurrences (e.g., --repeat weekly --count 10)",
    )
    recurrence_group.add_argument(
        "--until",
        help="End date for recurrence (YYYY-MM-DD)",
    )
    recurrence_group.add_argument(
        "--weekdays",
        help="Comma-separated weekdays for weekly recurrence (e.g., MO,WE,FR)",
    )
    recurrence_group.add_argument(
        "--rrule",
        help="Custom iCalendar RRULE string (e.g., 'FREQ=WEEKLY;BYDAY=MO,WE,FR;COUNT=10')",
    )

    parser.set_defaults(func=_handle_args)
