"""Test cases for the vcal import command."""

import subprocess
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from vcal_cli.main import main


@patch("vcal_cli.import_invite.subprocess.run")
def test_import_basic_invite(mock_run: MagicMock, tmp_path: Path) -> None:
    """Test importing a basic calendar invite."""
    # Mock khal import and vdirsyncer sync commands
    mock_run.return_value = subprocess.CompletedProcess(args=[], returncode=0)

    # Create a test calendar invite file
    invite_content = """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:test-uid@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Test Meeting
DESCRIPTION:This is a test meeting
LOCATION:Conference Room A
ORGANIZER:mailto:organizer@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:attendee@example.com
STATUS:CONFIRMED
TRANSP:OPAQUE
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "invite.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "import",
            str(invite_file),
            "--calendar",
            "TestCalendar",
        ],
    )

    assert result == 0

    # Verify khal import and vdirsyncer sync were called
    assert mock_run.call_count >= 2  # khal import + vdirsyncer sync
    khal_calls = [call for call in mock_run.call_args_list if call[0][0][0] == "khal"]
    assert len(khal_calls) >= 1


@patch("vcal_cli.import_invite.subprocess.run")
def test_import_from_email(mock_run: MagicMock, tmp_path: Path) -> None:
    """Test importing calendar invite from email."""
    mock_run.return_value = subprocess.CompletedProcess(args=[], returncode=0)

    email_content = """From: sender@example.com
To: recipient@example.com
Subject: Meeting Invitation
Content-Type: multipart/mixed; boundary="boundary"

--boundary
Content-Type: text/plain

You're invited to a meeting.

--boundary
Content-Type: text/calendar; charset=utf-8
Content-Transfer-Encoding: 7bit

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:email-invite@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Email Meeting
ORGANIZER:mailto:organizer@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:attendee@example.com
END:VEVENT
END:VCALENDAR
--boundary--"""

    email_file = tmp_path / "email.eml"
    email_file.write_text(email_content)

    result = main(
        [
            "import",
            str(email_file),
        ],
    )

    assert result == 0
    assert mock_run.call_count >= 2  # khal import + vdirsyncer sync


@patch("vcal_cli.import_invite.subprocess.run")
def test_import_invite_with_attendees(mock_run: MagicMock, tmp_path: Path) -> None:
    """Test importing invite with multiple attendees."""
    mock_run.return_value = subprocess.CompletedProcess(args=[], returncode=0)

    invite_content = """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:multi-attendee@example.com
DTSTART:20240320T100000Z
DTEND:20240320T110000Z
SUMMARY:Team Meeting
ORGANIZER;CN="John Doe":mailto:john@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION;CN="Alice":mailto:alice@example.com
ATTENDEE;PARTSTAT=ACCEPTED;CN="Bob":mailto:bob@example.com
ATTENDEE;PARTSTAT=DECLINED;CN="Charlie":mailto:charlie@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "multi.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "import",
            str(invite_file),
        ],
    )

    assert result == 0
    assert mock_run.call_count >= 2


@patch("vcal_cli.import_invite.subprocess.run")
def test_import_recurring_event(mock_run: MagicMock, tmp_path: Path) -> None:
    """Test importing recurring event."""
    mock_run.return_value = subprocess.CompletedProcess(args=[], returncode=0)

    invite_content = """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
BEGIN:VEVENT
UID:recurring@example.com
DTSTART;TZID=America/New_York:20240320T090000
DTEND;TZID=America/New_York:20240320T100000
RRULE:FREQ=WEEKLY;BYDAY=MO,WE,FR;COUNT=10
SUMMARY:Daily Standup
ORGANIZER:mailto:scrum@example.com
ATTENDEE:mailto:team@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "recurring.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "import",
            str(invite_file),
        ],
    )

    assert result == 0
    assert mock_run.call_count >= 2


@patch("vcal_cli.import_invite.subprocess.run")
def test_import_all_day_event(mock_run: MagicMock, tmp_path: Path) -> None:
    """Test importing all-day event."""
    mock_run.return_value = subprocess.CompletedProcess(args=[], returncode=0)

    invite_content = """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
BEGIN:VEVENT
UID:allday@example.com
DTSTART;VALUE=DATE:20240320
DTEND;VALUE=DATE:20240321
SUMMARY:Company Holiday
ORGANIZER:mailto:hr@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "allday.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "import",
            str(invite_file),
        ],
    )

    assert result == 0
    assert mock_run.call_count >= 2


@patch("vcal_cli.import_invite.subprocess.run")
def test_import_with_alarm(mock_run: MagicMock, tmp_path: Path) -> None:
    """Test importing event with alarm/reminder."""
    mock_run.return_value = subprocess.CompletedProcess(args=[], returncode=0)

    invite_content = """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
BEGIN:VEVENT
UID:alarm@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Important Meeting
ORGANIZER:mailto:boss@example.com
ATTENDEE:mailto:employee@example.com
BEGIN:VALARM
TRIGGER:-PT15M
ACTION:DISPLAY
DESCRIPTION:Reminder: Important Meeting in 15 minutes
END:VALARM
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "alarm.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "import",
            str(invite_file),
        ],
    )

    assert result == 0
    assert mock_run.call_count >= 2


def test_import_invalid_ics_file(tmp_path: Path) -> None:
    """Test importing invalid ICS file."""
    invite_content = """This is not a valid ICS file"""

    invite_file = tmp_path / "invalid.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "import",
            str(invite_file),
        ],
    )

    # Should fail due to invalid format
    assert result == 1


@patch("vcal_cli.import_invite.subprocess.run")
def test_import_multiple_events(mock_run: MagicMock, tmp_path: Path) -> None:
    """Test importing ICS with multiple events."""
    mock_run.return_value = subprocess.CompletedProcess(args=[], returncode=0)

    invite_content = """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
BEGIN:VEVENT
UID:event1@example.com
DTSTART:20240320T090000Z
DTEND:20240320T100000Z
SUMMARY:Morning Meeting
ORGANIZER:mailto:organizer@example.com
END:VEVENT
BEGIN:VEVENT
UID:event2@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Afternoon Meeting
ORGANIZER:mailto:organizer@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "multiple.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "import",
            str(invite_file),
        ],
    )

    assert result == 0
    # Should import both events in one ICS file
    assert mock_run.call_count >= 2  # 1 khal import + 1 vdirsyncer sync


def test_import_nonexistent_file(tmp_path: Path) -> None:  # noqa: ARG001
    """Test importing non-existent file."""
    result = main(
        [
            "import",
            "/non/existent/file.ics",
        ],
    )

    # Should fail due to missing file
    assert result == 1


@patch("vcal_cli.import_invite.subprocess.run")
def test_import_with_timezone_info(mock_run: MagicMock, tmp_path: Path) -> None:
    """Test importing event with timezone information."""
    mock_run.return_value = subprocess.CompletedProcess(args=[], returncode=0)

    invite_content = """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
BEGIN:VTIMEZONE
TZID:Europe/London
BEGIN:DAYLIGHT
TZOFFSETFROM:+0000
TZOFFSETTO:+0100
TZNAME:BST
DTSTART:19700329T010000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU
END:DAYLIGHT
BEGIN:STANDARD
TZOFFSETFROM:+0100
TZOFFSETTO:+0000
TZNAME:GMT
DTSTART:19701025T020000
RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
UID:tz-event@example.com
DTSTART;TZID=Europe/London:20240320T140000
DTEND;TZID=Europe/London:20240320T150000
SUMMARY:London Meeting
ORGANIZER:mailto:uk@example.com
ATTENDEE:mailto:participant@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "timezone.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "import",
            str(invite_file),
        ],
    )

    assert result == 0
    assert mock_run.call_count >= 2


@patch("vcal_cli.import_invite.subprocess.run")
def test_import_with_custom_calendar(mock_run: MagicMock, tmp_path: Path) -> None:
    """Test importing to a specific calendar."""
    mock_run.return_value = subprocess.CompletedProcess(args=[], returncode=0)

    invite_content = """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
BEGIN:VEVENT
UID:custom-cal@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Custom Calendar Event
ORGANIZER:mailto:test@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "custom.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "import",
            str(invite_file),
            "--calendar",
            "Work",
        ],
    )

    assert result == 0
    # Check that khal was called with the right calendar
    khal_calls = [call for call in mock_run.call_args_list if call[0][0][0] == "khal"]
    assert any("Work" in str(call) for call in khal_calls)


@patch("vcal_cli.import_invite.sys.stdin")
@patch("vcal_cli.import_invite.subprocess.run")
def test_import_from_stdin(
    mock_run: MagicMock,
    mock_stdin: MagicMock,
    tmp_path: Path,  # noqa: ARG001
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Test importing from stdin."""
    mock_run.return_value = subprocess.CompletedProcess(args=[], returncode=0)

    invite_content = """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
BEGIN:VEVENT
UID:stdin-test@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Stdin Test Event
ORGANIZER:mailto:test@example.com
END:VEVENT
END:VCALENDAR"""

    # Mock stdin.buffer.read() to return the content
    mock_stdin.buffer.read.return_value = invite_content.encode()

    result = main(
        [
            "import",
        ],
    )

    assert result == 0
    assert mock_run.call_count >= 2

    # Verify output
    captured = capsys.readouterr()
    assert "Successfully imported 1 calendar invite(s)" in captured.out


@patch("vcal_cli.import_invite.subprocess.run")
def test_import_with_rsvp(
    mock_run: MagicMock,
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Test importing invite with RSVP required - non-interactive mode."""
    mock_run.return_value = subprocess.CompletedProcess(args=[], returncode=0)

    email_content = """From: sender@example.com
To: recipient@example.com
Subject: Meeting Invitation
Content-Type: multipart/mixed; boundary="boundary"

--boundary
Content-Type: text/plain

Please RSVP for this meeting.

--boundary
Content-Type: text/calendar; charset=utf-8

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:rsvp-test@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:RSVP Required Meeting
ORGANIZER:mailto:organizer@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION;RSVP=TRUE:mailto:recipient@example.com
END:VEVENT
END:VCALENDAR
--boundary--"""

    email_file = tmp_path / "rsvp.eml"
    email_file.write_text(email_content)

    result = main(
        [
            "import",
            str(email_file),
        ],
    )

    assert result == 0
    captured = capsys.readouterr()
    # In non-interactive mode, it should just import without offering RSVP
    assert "Successfully imported 1 calendar invite(s)" in captured.out


@patch("vcal_cli.import_invite.input", side_effect=["y", "1"])  # Mock user input
@patch("vcal_cli.import_invite.sys.stdin.isatty", return_value=True)  # Allow interactive mode
@patch("vcal_cli.import_invite.sys.stdout.isatty", return_value=True)  # Allow interactive mode
@patch("vcal_cli.import_invite.run_reply", return_value=0)  # Mock the reply function
@patch("vcal_cli.import_invite.subprocess.run")
def test_import_with_rsvp_interactive(  # noqa: PLR0913
    mock_run: MagicMock,
    mock_run_reply: MagicMock,
    mock_stdout_tty: MagicMock,  # noqa: ARG001
    mock_stdin_tty: MagicMock,  # noqa: ARG001
    mock_input: MagicMock,  # noqa: ARG001
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Test importing invite with RSVP in interactive mode."""
    # Mock khal import and vdirsyncer subprocess calls
    mock_run.return_value = subprocess.CompletedProcess(args=[], returncode=0)

    email_content = """From: sender@example.com
To: recipient@example.com
Subject: Meeting Invitation
Content-Type: multipart/mixed; boundary="boundary"

--boundary
Content-Type: text/plain

Please RSVP for this meeting.

--boundary
Content-Type: text/calendar; charset=utf-8

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:rsvp-interactive@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:RSVP Interactive Meeting
ORGANIZER:mailto:organizer@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION;RSVP=TRUE:mailto:recipient@example.com
END:VEVENT
END:VCALENDAR
--boundary--"""

    email_file = tmp_path / "rsvp-interactive.eml"
    email_file.write_text(email_content)

    result = main(
        [
            "import",
            str(email_file),
        ],
    )

    assert result == 0

    # Verify subprocess.run was called for khal and vdirsyncer
    assert mock_run.call_count >= 2  # khal import + vdirsyncer sync

    # Check that run_reply was called with correct config
    mock_run_reply.assert_called_once()
    reply_config = mock_run_reply.call_args[0][0]
    assert reply_config.status == "accept"
    assert reply_config.dry_run is False

    captured = capsys.readouterr()
    assert "Would you like to respond?" in captured.out
    assert "RSVP response sent successfully." in captured.out
