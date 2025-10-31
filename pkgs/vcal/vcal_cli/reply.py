"""Reply to calendar invites with RSVP responses."""

from __future__ import annotations

import email
import email.utils
import subprocess
import sys
from dataclasses import dataclass
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from pathlib import Path
from typing import TYPE_CHECKING

from icalendar import Calendar, Event, vCalAddress, vText

if TYPE_CHECKING:
    import argparse


@dataclass
class ReplyConfig:
    """Configuration for reply command."""

    status: str  # accept, decline, tentative
    file_path: str | None = None
    comment: str | None = None
    dry_run: bool = False


def _extract_recipient_email(msg: email.message.Message) -> str | None:
    """Extract recipient email from message headers."""
    if "To" in msg:
        to_header = msg["To"]
        # Parse the email address from the To field
        _name, addr = email.utils.parseaddr(to_header)
        if addr:
            return addr
    return None


def _extract_calendar_from_part(part: email.message.Message) -> Calendar | None:
    """Extract calendar from a message part if it's a calendar type."""
    if part.get_content_type() not in ["text/calendar", "application/ics"]:
        return None

    try:
        cal_data = part.get_payload(decode=True)
        if isinstance(cal_data, bytes):
            cal = Calendar.from_ical(cal_data.decode("utf-8"))
            if isinstance(cal, Calendar):
                return cal
    except (ValueError, TypeError):
        pass

    return None


def extract_calendar_from_email(email_content: str) -> tuple[Calendar | None, str | None]:
    """Extract calendar and recipient email from email message."""
    msg = email.message_from_string(email_content)
    to_email = _extract_recipient_email(msg)

    # Try to extract from message parts
    for part in msg.walk():
        cal = _extract_calendar_from_part(part)
        if cal:
            return cal, to_email

    # Try to parse direct calendar data
    if "BEGIN:VCALENDAR" in email_content:
        try:
            result = Calendar.from_ical(email_content)
            if isinstance(result, Calendar):
                return result, to_email
        except (ValueError, TypeError):
            pass

    return None, to_email


def create_reply(  # noqa: C901
    original_cal: Calendar,
    status: str,
    user_email: str,
    comment: str | None = None,
) -> Calendar:
    """Create REPLY calendar for RSVP response."""
    reply_cal = Calendar()
    reply_cal.add("prodid", "-//reply-calendar-invite//")
    reply_cal.add("version", "2.0")
    reply_cal.add("method", "REPLY")

    # Extract name from email or use a default
    user_name = user_email.split("@")[0].replace(".", " ").title()

    # Find the original event
    for component in original_cal.walk():
        if component.name == "VEVENT":
            # Create reply event with minimal required fields
            reply_event = Event()

            # Copy required fields
            for field in ["uid", "sequence", "dtstamp", "dtstart", "dtend", "summary"]:
                if field in component:
                    reply_event.add(field, component[field])

            # Set organizer from original
            if "organizer" in component:
                reply_event.add("organizer", component["organizer"])

            # Find our attendee entry and update status
            attendees = component.get("attendee", [])
            if not isinstance(attendees, list):
                attendees = [attendees]

            found_self = False
            for attendee in attendees:
                attendee_email = str(attendee).replace("mailto:", "").lower()
                if attendee_email == user_email.lower():
                    # Update our attendance status
                    reply_attendee = vCalAddress(f"mailto:{user_email}")
                    reply_attendee.params["cn"] = vText(user_name)
                    reply_attendee.params["partstat"] = vText(status)
                    reply_attendee.params["rsvp"] = vText("FALSE")
                    if "role" in attendee.params:
                        reply_attendee.params["role"] = attendee.params["role"]
                    reply_event.add("attendee", reply_attendee)
                    found_self = True
                    break

            # If we weren't in the attendee list, add ourselves
            if not found_self:
                reply_attendee = vCalAddress(f"mailto:{user_email}")
                reply_attendee.params["cn"] = vText(user_name)
                reply_attendee.params["partstat"] = vText(status)
                reply_attendee.params["rsvp"] = vText("FALSE")
                reply_attendee.params["role"] = vText("REQ-PARTICIPANT")
                reply_event.add("attendee", reply_attendee)

            # Add comment if provided
            if comment:
                reply_event.add("comment", comment)

            reply_cal.add_component(reply_event)
            break

    return reply_cal


def send_reply(
    reply_cal: Calendar,
    organizer_email: str,
    event_summary: str,
    status: str,
    user_email: str,
) -> bool:
    """Send REPLY via msmtp."""
    user_name = user_email.split("@")[0].replace(".", " ").title()

    # Create email message
    msg = MIMEMultipart("mixed")

    # Map status to human-readable response
    status_map = {
        "ACCEPTED": "Accepted",
        "DECLINED": "Declined",
        "TENTATIVE": "Tentative",
    }
    human_status = status_map.get(status, status)

    msg["Subject"] = f"Re: {event_summary} - {human_status}"
    msg["From"] = f"{user_name} <{user_email}>"
    msg["To"] = organizer_email
    msg["Date"] = email.utils.formatdate(localtime=True)

    # Body
    body = MIMEText(
        f"This is an automatic reply to your meeting invitation.\n\nStatus: {human_status}\n",
    )
    msg.attach(body)

    # Calendar attachment
    cal_part = MIMEText(reply_cal.to_ical().decode(), "calendar")
    cal_part.add_header("Content-Disposition", 'attachment; filename="reply.ics"')
    cal_part.set_param("method", "REPLY")
    msg.attach(cal_part)

    # Send via msmtp
    try:
        proc = subprocess.Popen(
            ["msmtp", "-t", "-a", "default"],  # noqa: S607
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        _stdout, stderr = proc.communicate(msg.as_string())
    except (OSError, subprocess.SubprocessError) as e:
        print(f"Error sending reply: {e}", file=sys.stderr)
        return False
    else:
        if proc.returncode != 0:
            print(f"Error sending reply: {stderr}", file=sys.stderr)
            return False
        return True


def read_email_content(file_path: str | None) -> str:
    """Read email content from file or stdin."""
    if file_path:
        if not Path(file_path).exists():
            print(f"Error: File '{file_path}' not found", file=sys.stderr)
            sys.exit(1)
        with Path(file_path).open() as f:
            return f.read()
    else:
        return sys.stdin.read()


def extract_event_info(original_cal: Calendar) -> tuple[str | None, str]:
    """Extract organizer email and event summary from calendar."""
    organizer_email = None
    event_summary = "Meeting"

    for component in original_cal.walk():
        if component.name == "VEVENT":
            if "organizer" in component:
                organizer_email = str(component["organizer"]).replace("mailto:", "")
            if "summary" in component:
                event_summary = str(component["summary"])
            break

    return organizer_email, event_summary


def run(config: ReplyConfig) -> int:
    """Run the reply command with the given configuration."""
    # Map user-friendly status to iCalendar format
    status_map = {
        "accept": "ACCEPTED",
        "decline": "DECLINED",
        "tentative": "TENTATIVE",
    }
    ical_status = status_map[config.status]

    # Read input
    email_content = read_email_content(config.file_path)

    # Extract calendar from email
    original_cal, user_email = extract_calendar_from_email(email_content)
    if not original_cal:
        print("Error: No calendar invite found in input", file=sys.stderr)
        return 1

    if not user_email:
        print("Error: No recipient email found in input", file=sys.stderr)
        return 1

    # Find organizer email and event summary
    organizer_email, event_summary = extract_event_info(original_cal)

    if not organizer_email:
        print("Error: No organizer found in calendar invite", file=sys.stderr)
        return 1

    # Create reply
    reply_cal = create_reply(original_cal, ical_status, user_email, config.comment)

    if config.dry_run:
        print(f"Would send reply to: {organizer_email}")
        print(f"Status: {ical_status}")
        print("\nReply calendar:")
        print(reply_cal.to_ical().decode())
    elif send_reply(reply_cal, organizer_email, event_summary, ical_status, user_email):
        print(f"Successfully sent {config.status} reply to {organizer_email}")
    else:
        return 1

    return 0


def _handle_args(args: argparse.Namespace) -> int:
    """Handle parsed arguments and run the command."""
    config = ReplyConfig(
        status=args.status,
        file_path=args.file,
        comment=args.comment,
        dry_run=args.dry_run,
    )
    return run(config)


def register_parser(subparsers: argparse._SubParsersAction) -> None:
    """Register the reply subcommand."""
    parser = subparsers.add_parser(
        "reply",
        help="Reply to calendar invites with RSVP responses",
        description="Send RSVP responses (accept/decline/tentative) to calendar invitations",
    )

    parser.add_argument(
        "status",
        choices=["accept", "decline", "tentative"],
        help="RSVP response status",
    )
    parser.add_argument(
        "-c",
        "--comment",
        help="Optional comment to include with response",
    )
    parser.add_argument(
        "-n",
        "--dry-run",
        action="store_true",
        help="Print the reply instead of sending",
    )
    parser.add_argument(
        "file",
        nargs="?",
        help="Email file containing invite (reads from stdin if not provided)",
    )

    parser.set_defaults(func=_handle_args)
