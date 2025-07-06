#!/usr/bin/env python3
"""
Generate ICS calendar invites and send them via msmtp.
Supports attendees with names/emails and meeting links.
"""

import argparse
import os
import pwd
import re
import socket
import subprocess
import sys
import tempfile
import uuid
from datetime import datetime, timedelta
from email import encoders
from email.mime.base import MIMEBase
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from pathlib import Path

import pytz
from icalendar import Alarm, Calendar, Event, vCalAddress, vText
from pytz import timezone


def parse_attendees(attendees_str: str) -> list[tuple[str, str]]:
    """Parse attendee string format: 'Name <email>' or just 'email'."""
    attendees = []
    for attendee_str in attendees_str.split(","):
        attendee = attendee_str.strip()
        match = re.match(r"^(.+?)\s*<(.+?)>$", attendee)
        if match:
            name, email = match.groups()
            attendees.append((name.strip(), email.strip()))
        elif "@" in attendee:
            # Just an email address
            attendees.append(("", attendee))
        else:
            print(f"Warning: Invalid attendee format: {attendee}", file=sys.stderr)
    return attendees


def create_calendar_invite(
    summary: str,
    start: datetime,
    end: datetime,
    organizer_name: str,
    organizer_email: str,
    attendees: list[tuple[str, str]],
    location: str | None = None,
    meeting_link: str | None = None,
    description: str | None = None,
    reminder_minutes: int = 15,
    tz: str = "UTC",
) -> Calendar:
    """Create an ICS calendar invite."""
    cal = Calendar()
    cal.add("prodid", "-//Calendar Invite Generator//mxm.dk//")
    cal.add("version", "2.0")
    cal.add("method", "REQUEST")

    event = Event()

    # Basic event properties
    event.add("summary", summary)
    event.add("dtstart", start)
    event.add("dtend", end)
    event.add("dtstamp", datetime.now(pytz.utc))
    event.add("uid", f"{uuid.uuid4()}@calendar-invite-generator")
    event.add("sequence", 0)
    event.add("status", "CONFIRMED")
    event.add("transp", "OPAQUE")

    # Location and meeting link
    if location:
        event.add("location", location)

    # Description with meeting link
    desc_parts = []
    if description:
        desc_parts.append(description)
    if meeting_link:
        desc_parts.append(f"\nJoin meeting: {meeting_link}")
    if desc_parts:
        event.add("description", "\n".join(desc_parts))

    # Organizer
    organizer = vCalAddress(f"mailto:{organizer_email}")
    organizer.params["cn"] = vText(organizer_name)
    organizer.params["role"] = vText("CHAIR")
    event.add("organizer", organizer)

    # Add organizer as attendee (so it appears in their calendar)
    org_attendee = vCalAddress(f"mailto:{organizer_email}")
    org_attendee.params["cn"] = vText(organizer_name)
    org_attendee.params["partstat"] = vText("ACCEPTED")
    org_attendee.params["role"] = vText("REQ-PARTICIPANT")
    event.add("attendee", org_attendee, encode=0)

    # Attendees
    for name, email in attendees:
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
    alarm.add("description", f"Reminder: {summary}")
    alarm.add("trigger", timedelta(minutes=-reminder_minutes))
    event.add_component(alarm)

    cal.add_component(event)
    return cal


def send_invite_email(
    cal: Calendar,
    summary: str,
    start: datetime,
    end: datetime,
    organizer_name: str,
    organizer_email: str,
    attendees: list[tuple[str, str]],
    meeting_link: str | None = None,
    dry_run: bool = False,
) -> bool:
    """Send calendar invite via msmtp."""
    # Create email message
    msg = MIMEMultipart("mixed")
    msg["Subject"] = f"Invitation: {summary}"
    msg["From"] = f"{organizer_name} <{organizer_email}>"

    # Build recipient list
    recipients = []
    to_list = []
    for name, email in attendees:
        recipients.append(email)
        if name:
            to_list.append(f"{name} <{email}>")
        else:
            to_list.append(email)

    msg["To"] = ", ".join(to_list)

    # Create email body
    body_text = f"""You have been invited to: {summary}

When: {start.strftime("%A, %B %d, %Y")}
Time: {start.strftime("%I:%M %p")} - {end.strftime("%I:%M %p %Z")}"""

    if meeting_link:
        body_text += f"\n\nJoin meeting: {meeting_link}"

    body_text += "\n\nPlease see the attached calendar invitation for details."

    # Add text part
    msg.attach(MIMEText(body_text, "plain"))

    # Add calendar part
    cal_part = MIMEBase("text", "calendar")
    cal_part.set_payload(cal.to_ical())
    cal_part.add_header("Content-Disposition", 'attachment; filename="invite.ics"')
    cal_part.add_header(
        "Content-Type", 'text/calendar; charset="UTF-8"; method=REQUEST'
    )
    encoders.encode_base64(cal_part)
    msg.attach(cal_part)

    # Send email
    if dry_run:
        print("\n--- DRY RUN: Email would be sent with the following ---")
        print(f"From: {msg['From']}")
        print(f"To: {msg['To']}")
        print(f"Subject: {msg['Subject']}")
        print("\nBody:")
        print(body_text)
        print("\nAttachment: invite.ics")
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
                cmd, check=False, stdin=f, capture_output=True, text=True
            )

        # Clean up temp file
        Path(temp_file).unlink()

        if result.returncode != 0:
            print(f"Error sending email: {result.stderr}", file=sys.stderr)
            return False

    except OSError as e:
        print(f"Error sending email: {e}", file=sys.stderr)
        return False
    else:
        print(f"✓ Calendar invite sent to {len(recipients)} recipient(s)")
        return True


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate and send ICS calendar invites via msmtp",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Simple meeting
  %(prog)s -s "Team Meeting" -d 60 -a "john@example.com,Jane Doe <jane@example.com>"

  # Meeting with Zoom link
  %(prog)s -s "Project Review" -d 90 -a "team@example.com" \\
    -l "https://zoom.us/j/123456789" --meeting-link "https://zoom.us/j/123456789"

  # Specific time and timezone
  %(prog)s -s "Client Call" --start "2024-01-15 14:00" -d 30 \\
    -a "Client Name <client@company.com>" --timezone "America/New_York"

  # Save ICS file without sending
  %(prog)s -s "Meeting" -a "test@example.com" --no-send -o meeting.ics
        """,
    )

    parser.add_argument("-s", "--summary", required=True, help="Meeting title/summary")
    parser.add_argument(
        "--start", help="Start time (YYYY-MM-DD HH:MM), defaults to next hour"
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
        "--organizer-email", help="Your email (defaults to $EMAIL or user@hostname)"
    )
    parser.add_argument("-l", "--location", help="Meeting location")
    parser.add_argument("--meeting-link", help="Meeting URL (Zoom, Teams, etc.)")
    parser.add_argument("--description", help="Additional meeting description")
    parser.add_argument(
        "--reminder", type=int, default=15, help="Reminder minutes before (default: 15)"
    )
    parser.add_argument("--timezone", default="UTC", help="Timezone (default: UTC)")
    parser.add_argument(
        "-o", "--output", help="Save ICS to file (in addition to sending)"
    )
    parser.add_argument(
        "--no-send", action="store_true", help="Only create ICS file, do not send"
    )
    parser.add_argument(
        "--dry-run", action="store_true", help="Show what would be sent without sending"
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
        "--no-local-save", action="store_true", help="Do not save to local calendar"
    )

    args = parser.parse_args()

    # Parse timezone
    try:
        tz = timezone(args.timezone)
    except pytz.exceptions.UnknownTimeZoneError:
        print(f"Error: Unknown timezone '{args.timezone}'", file=sys.stderr)
        print(
            "Use values like: UTC, America/New_York, Europe/London, Asia/Tokyo",
            file=sys.stderr,
        )
        sys.exit(1)

    # Parse start time
    if args.start:
        try:
            naive_dt = datetime.strptime(args.start, "%Y-%m-%d %H:%M")  # noqa: DTZ007
            start = tz.localize(naive_dt)
        except ValueError:
            print(
                "Error: Invalid start time format. Use YYYY-MM-DD HH:MM",
                file=sys.stderr,
            )
            sys.exit(1)
    else:
        # Default to next hour
        now = datetime.now(tz)
        start = now.replace(minute=0, second=0, microsecond=0) + timedelta(hours=1)

    # Calculate end time
    end = start + timedelta(minutes=args.duration)

    # Parse attendees
    attendees = parse_attendees(args.attendees)
    if not attendees:
        print("Error: No valid attendees provided", file=sys.stderr)
        sys.exit(1)

    # Get organizer info
    organizer_name = args.organizer_name
    if not organizer_name:
        try:
            organizer_name = (
                pwd.getpwuid(os.getuid()).pw_gecos.split(",")[0] or os.getlogin()
            )
        except (KeyError, OSError):
            organizer_name = "Meeting Organizer"

    organizer_email = args.organizer_email
    if not organizer_email:
        # Try to get from environment or construct default
        organizer_email = os.environ.get("EMAIL")
        if not organizer_email:
            username = os.getlogin()
            hostname = socket.gethostname()
            organizer_email = f"{username}@{hostname}"
            print(
                f"Warning: No organizer email specified, using: {organizer_email}",
                file=sys.stderr,
            )

    # Create calendar invite
    cal = create_calendar_invite(
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
    )

    # Save ICS file if requested
    if args.output or args.no_send:
        if args.output:
            output_file = args.output
        else:
            # Generate filename from summary and date
            safe_summary = (
                re.sub(r"[^\w\s-]", "", args.summary).strip().replace(" ", "-")
            )
            date_str = start.strftime("%Y%m%d")
            output_file = f"{safe_summary}-{date_str}.ics"

        output_path = Path(output_file)
        with output_path.open("wb") as f:
            f.write(cal.to_ical())
        print(f"✓ Calendar invite saved: {output_path}")

    # Print meeting details
    print(f"\nMeeting: {args.summary}")
    print(
        f"Time: {start.strftime('%Y-%m-%d %H:%M')} - "
        f"{end.strftime('%H:%M')} {args.timezone}"
    )
    print(f"Duration: {args.duration} minutes")
    print(f"Organizer: {organizer_name} <{organizer_email}>")
    print(f"Attendees: {len(attendees)}")
    for name, email in attendees:
        if name:
            print(f"  - {name} <{email}>")
        else:
            print(f"  - {email}")
    if args.meeting_link:
        print(f"Meeting link: {args.meeting_link}")

    # Save to local calendar if not disabled
    if not args.no_local_save:
        calendar_dir = Path(args.calendar_dir).expanduser()
        if calendar_dir.is_dir():
            # Generate unique filename using UID
            event_uid = cal.walk("vevent")[0]["UID"]
            calendar_file = calendar_dir / f"{event_uid}.ics"
            try:
                with calendar_file.open("wb") as f:
                    f.write(cal.to_ical())
                print(f"\n✓ Event saved to local calendar: {calendar_file}")
            except OSError as e:
                print(
                    f"Warning: Could not save to local calendar: {e}", file=sys.stderr
                )
        else:
            print(
                f"Warning: Calendar directory not found: {calendar_dir}",
                file=sys.stderr,
            )

    # Send email if not disabled
    if not args.no_send:
        print()  # Empty line before sending
        success = send_invite_email(
            cal=cal,
            summary=args.summary,
            start=start,
            end=end,
            organizer_name=organizer_name,
            organizer_email=organizer_email,
            attendees=attendees,
            meeting_link=args.meeting_link,
            dry_run=args.dry_run,
        )

        if not success and not args.dry_run:
            sys.exit(1)


if __name__ == "__main__":
    main()
