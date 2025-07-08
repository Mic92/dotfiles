"""Import calendar invites from email or .ics files."""

from __future__ import annotations

import email
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING

from .reply import ReplyConfig
from .reply import run as run_reply

if TYPE_CHECKING:
    import argparse


@dataclass
class ImportConfig:
    """Configuration for import command."""

    file_path: str | None = None
    calendar: str = "Personal"


def extract_calendar_from_email(email_content: bytes) -> list[bytes]:
    """Extract calendar parts from email message."""
    msg = email.message_from_bytes(email_content)
    calendars: list[bytes] = []

    for part in msg.walk():
        if part.get_content_type() in ["text/calendar", "application/ics"]:
            cal_data = part.get_payload(decode=True)
            if isinstance(cal_data, bytes) and b"BEGIN:VCALENDAR" in cal_data:
                calendars.append(cal_data)
        # Also check for .ics attachments
        filename = part.get_filename()
        if filename and filename.lower().endswith(".ics"):
            cal_data = part.get_payload(decode=True)
            if isinstance(cal_data, bytes) and b"BEGIN:VCALENDAR" in cal_data:
                calendars.append(cal_data)

    return calendars


def import_to_khal(calendar_data: bytes, calendar_name: str) -> bool:
    """Import calendar data using khal."""
    try:
        with tempfile.NamedTemporaryFile(suffix=".ics", delete=False) as f:
            f.write(calendar_data)
            temp_path = f.name

        result = subprocess.run(
            ["khal", "import", "--batch", "-a", calendar_name, temp_path],  # noqa: S607
            check=False,
            capture_output=True,
            text=True,
        )

        Path(temp_path).unlink(missing_ok=True)
    except (OSError, subprocess.SubprocessError) as e:
        print(f"Error importing calendar: {e}", file=sys.stderr)
        return False
    else:
        return result.returncode == 0


def sync_calendar() -> None:
    """Sync calendar with server using vdirsyncer."""
    subprocess.run(["vdirsyncer", "sync"], check=False, capture_output=True)  # noqa: S607


def offer_rsvp(email_path: str | None, email_content: bytes | None) -> None:
    """Offer RSVP option for calendar invites."""
    if not sys.stdin.isatty() or not sys.stdout.isatty():
        return

    print("\nThis invitation requests an RSVP. Would you like to respond? (y/n)")
    reply = input().strip().lower()

    if reply == "y":
        print("Please select your response:")
        print("1) Accept")
        print("2) Decline")
        print("3) Tentative")
        choice = input("Your choice (1-3): ").strip()

        status_map = {"1": "accept", "2": "decline", "3": "tentative"}
        if choice not in status_map:
            print("Invalid choice. Skipping RSVP.")
            return

        status = status_map[choice]
        print("Sending RSVP response...")

        # Create reply config
        reply_config = ReplyConfig(
            status=status,
            file_path=email_path,
            dry_run=False,
        )

        # If we have email content but no path, we need to save it temporarily
        temp_file = None
        try:
            if not email_path and email_content:
                with tempfile.NamedTemporaryFile(mode="wb", suffix=".eml", delete=False) as f:
                    f.write(email_content)
                    temp_file = f.name
                reply_config.file_path = temp_file

            # Call the reply function directly
            result = run_reply(reply_config)

            if result == 0:
                print("RSVP response sent successfully.")
            else:
                print("Failed to send RSVP response", file=sys.stderr)
        finally:
            # Clean up temp file if created
            if temp_file:
                Path(temp_file).unlink(missing_ok=True)


def process_input(file_path: str | None) -> tuple[list[bytes], bool, bytes | None]:
    """Process input and extract calendars."""
    is_email = False
    email_content = None
    calendars = []

    if file_path:
        path = Path(file_path)
        if not path.exists():
            print(f"Error: File '{file_path}' not found", file=sys.stderr)
            return [], False, None

        content = path.read_bytes()

        if path.suffix.lower() == ".ics":
            if b"BEGIN:VCALENDAR" in content:
                calendars.append(content)
        else:
            email_calendars = extract_calendar_from_email(content)
            if email_calendars:
                calendars.extend(email_calendars)
                is_email = True
                email_content = content
    else:
        content = sys.stdin.buffer.read()
        email_calendars = extract_calendar_from_email(content)
        if email_calendars:
            calendars.extend(email_calendars)
            is_email = True
            email_content = content
        elif b"BEGIN:VCALENDAR" in content and not content.startswith(b"Content-Type:"):
            calendars.append(content)

    return calendars, is_email, email_content


def import_calendars(calendars: list[bytes], calendar_name: str) -> tuple[int, bool]:
    """Import calendar data and return count and RSVP status."""
    imported = 0
    has_rsvp = False

    for cal_data in calendars:
        # Check for RSVP
        if b"RSVP:TRUE" in cal_data or b"RSVP=TRUE" in cal_data:
            has_rsvp = True

        if import_to_khal(cal_data, calendar_name):
            imported += 1
            print("Successfully imported calendar")
        else:
            print("Warning: Failed to import calendar", file=sys.stderr)

    return imported, has_rsvp


def run(config: ImportConfig) -> int:
    """Run the import command with the given configuration."""
    # Process input
    calendars, is_email, email_content = process_input(config.file_path)

    if not calendars:
        print("No .ics files found in the input", file=sys.stderr)
        return 1

    # Import calendars
    imported, has_rsvp = import_calendars(calendars, config.calendar)

    if imported == 0:
        return 1

    # Sync with server
    sync_calendar()

    print(f"Successfully imported {imported} calendar invite(s) to {config.calendar}")

    # Offer RSVP if this was an email with RSVP request
    if is_email and has_rsvp:
        if config.file_path:
            offer_rsvp(config.file_path, None)
        else:
            offer_rsvp(None, email_content)

    return 0


def _handle_args(args: argparse.Namespace) -> int:
    """Handle parsed arguments and run the command."""
    config = ImportConfig(
        file_path=args.file,
        calendar=args.calendar,
    )
    return run(config)


def register_parser(subparsers: argparse._SubParsersAction) -> None:
    """Register the import subcommand."""
    parser = subparsers.add_parser(
        "import",
        help="Import calendar invites from email or .ics files",
        description="Import calendar invitations into your local calendar",
    )

    parser.add_argument(
        "-c",
        "--calendar",
        default="Personal",
        help="Calendar name (default: Personal)",
    )
    parser.add_argument(
        "file",
        nargs="?",
        help="File to import (reads from stdin if not provided)",
    )

    parser.set_defaults(func=_handle_args)
