"""Test cases for the vcal reply command."""

import io
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from vcal_cli.main import main


@patch("vcal_cli.reply.subprocess.Popen")
def test_reply_accept_invite(
    mock_popen: MagicMock,
    tmp_path: Path,
) -> None:
    """Test replying to accept an invite."""
    # Mock the msmtp subprocess
    mock_process = MagicMock()
    mock_process.communicate.return_value = ("", "")
    mock_process.returncode = 0
    mock_popen.return_value = mock_process

    # Create a test calendar invite file with email headers
    invite_content = """From: sender@example.com
To: testuser@example.com
Subject: Meeting Invitation
Content-Type: text/calendar

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:test-uid@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Test Meeting
ORGANIZER:mailto:organizer@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:me@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:other@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "invite.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "reply",
            "accept",
            str(invite_file),
        ],
    )

    assert result == 0

    # Verify msmtp was called
    mock_popen.assert_called_once()
    args = mock_popen.call_args[0][0]
    assert args == ["msmtp", "-t", "-a", "default"]

    # Check the email content sent to msmtp
    email_content = mock_process.communicate.call_args[0][0]
    assert "To: organizer@example.com" in email_content
    assert "METHOD:REPLY" in email_content
    assert "PARTSTAT=ACCEPTED" in email_content

    # Additional checks for email structure
    assert "From: Testuser <testuser@example.com>" in email_content
    assert "Subject: Re: Test Meeting - Accepted" in email_content
    assert "This is an automatic reply to your meeting invitation." in email_content
    assert "Status: Accepted" in email_content

    # Check calendar attachment
    assert "Content-Type: text/calendar" in email_content
    assert 'attachment; filename="reply.ics"' in email_content
    assert "BEGIN:VCALENDAR" in email_content
    assert "METHOD:REPLY" in email_content
    assert "UID:test-uid@example.com" in email_content


def test_reply_accept_invite_dry_run(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    """Test replying with dry-run option."""
    invite_content = """From: sender@example.com
To: attendee@example.com
Subject: Meeting Invitation
Content-Type: text/calendar

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:dry-run-test@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Dry Run Meeting
ORGANIZER:mailto:organizer@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:attendee@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "dryrun.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "reply",
            "accept",
            "--dry-run",
            str(invite_file),
        ],
    )

    assert result == 0

    captured = capsys.readouterr()
    assert "Would send reply to: organizer@example.com" in captured.out
    assert "Status: ACCEPTED" in captured.out
    assert "METHOD:REPLY" in captured.out


def test_reply_decline_invite(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    """Test replying to decline an invite."""
    invite_content = """From: sender@example.com
To: employee@example.com
Subject: Meeting Invitation
Content-Type: text/calendar

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:decline-test@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Meeting to Decline
ORGANIZER:mailto:boss@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:employee@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "decline.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "reply",
            "decline",
            "--dry-run",
            str(invite_file),
        ],
    )

    assert result == 0

    captured = capsys.readouterr()
    assert "Status: DECLINED" in captured.out
    assert "METHOD:REPLY" in captured.out


def test_reply_tentative_invite(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    """Test replying tentatively to an invite."""
    invite_content = """From: sender@example.com
To: maybe@example.com
Subject: Meeting Invitation
Content-Type: text/calendar

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:tentative-test@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Maybe Meeting
ORGANIZER:mailto:organizer@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:maybe@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "tentative.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "reply",
            "tentative",
            "--dry-run",
            str(invite_file),
        ],
    )

    assert result == 0

    captured = capsys.readouterr()
    assert "Status: TENTATIVE" in captured.out


@patch("vcal_cli.reply.subprocess.Popen")
def test_reply_send_via_email(
    mock_popen: MagicMock,
    tmp_path: Path,
) -> None:
    """Test sending reply via email."""
    mock_process = MagicMock()
    mock_process.communicate.return_value = ("", "")
    mock_process.returncode = 0
    mock_popen.return_value = mock_process

    invite_content = """From: sender@example.com
To: testuser@example.com
Subject: Meeting Invitation
Content-Type: text/calendar

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:email-test@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Email Reply Test
ORGANIZER;CN="The Organizer":mailto:organizer@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:attendee@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "email.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "reply",
            "accept",
            str(invite_file),
        ],
    )

    assert result == 0

    # Verify msmtp was called
    mock_popen.assert_called_once()

    # Check the email content sent to msmtp
    email_content = mock_process.communicate.call_args[0][0]

    # Verify email headers
    assert "To: organizer@example.com" in email_content
    assert "From: Testuser <testuser@example.com>" in email_content
    assert "Subject: Re: Email Reply Test - Accepted" in email_content

    # Verify email body
    assert "This is an automatic reply to your meeting invitation." in email_content
    assert "Status: Accepted" in email_content

    # Verify calendar attachment
    assert "Content-Type: text/calendar" in email_content
    assert 'attachment; filename="reply.ics"' in email_content
    assert "BEGIN:VCALENDAR" in email_content
    assert "METHOD:REPLY" in email_content
    assert "PARTSTAT=ACCEPTED" in email_content
    assert "UID:email-test@example.com" in email_content


def test_reply_with_comment(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    """Test replying with a comment."""
    invite_content = """From: sender@example.com
To: attendee@example.com
Subject: Meeting Invitation
Content-Type: text/calendar

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:comment-test@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Comment Test Meeting
ORGANIZER:mailto:organizer@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:attendee@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "comment.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "reply",
            "tentative",
            "--comment",
            "I might be 10 minutes late",
            "--dry-run",
            str(invite_file),
        ],
    )

    assert result == 0

    captured = capsys.readouterr()
    assert "COMMENT:I might be 10 minutes late" in captured.out
    assert "Status: TENTATIVE" in captured.out


def test_reply_to_recurring_event(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    """Test replying to a recurring event."""
    invite_content = """From: sender@example.com
To: dev@example.com
Subject: Meeting Invitation
Content-Type: text/calendar

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:recurring-reply@example.com
DTSTART;TZID=Europe/Berlin:20240320T090000
DTEND;TZID=Europe/Berlin:20240320T100000
RRULE:FREQ=WEEKLY;BYDAY=MO,WE,FR
SUMMARY:Weekly Standup
ORGANIZER:mailto:scrum@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:dev@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "recurring.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "reply",
            "accept",
            "--dry-run",
            str(invite_file),
        ],
    )

    assert result == 0

    captured = capsys.readouterr()
    # Reply includes acceptance status
    assert "Status: ACCEPTED" in captured.out


def test_reply_missing_organizer(tmp_path: Path) -> None:
    """Test replying to invite without organizer."""
    invite_content = """From: sender@example.com
To: attendee@example.com
Subject: Meeting Invitation
Content-Type: text/calendar

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:no-organizer@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:No Organizer Meeting
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:attendee@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "no-org.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "reply",
            "accept",
            str(invite_file),
        ],
    )

    # Should fail because there's no organizer to send the reply to
    assert result == 1


def test_reply_invalid_ics_file(tmp_path: Path) -> None:
    """Test replying to invalid ICS file."""
    invite_content = """This is not a valid ICS file"""

    invite_file = tmp_path / "invalid.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "reply",
            "accept",
            str(invite_file),
        ],
    )

    # Should fail due to invalid format
    assert result == 1


def test_reply_missing_to_header(tmp_path: Path) -> None:
    """Test replying to calendar invite without To header."""
    invite_content = """From: sender@example.com
Subject: Meeting Invitation
Content-Type: text/calendar

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:no-to-header@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:No To Header Meeting
ORGANIZER:mailto:organizer@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:attendee@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "no-to.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "reply",
            "accept",
            str(invite_file),
        ],
    )

    # Should fail because there's no recipient email in the To header
    assert result == 1


@patch("vcal_cli.reply.subprocess.Popen")
def test_reply_msmtp_failure(
    mock_popen: MagicMock,
    tmp_path: Path,
) -> None:
    """Test handling msmtp failure."""
    mock_process = MagicMock()
    mock_process.communicate.return_value = ("", "msmtp: connection failed")
    mock_process.returncode = 1
    mock_popen.return_value = mock_process

    invite_content = """From: sender@example.com
To: testuser@example.com
Subject: Meeting Invitation
Content-Type: text/calendar

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:msmtp-fail@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:MSMTP Fail Test
ORGANIZER:mailto:organizer@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:attendee@example.com
END:VEVENT
END:VCALENDAR"""

    invite_file = tmp_path / "msmtp-fail.ics"
    invite_file.write_text(invite_content)

    result = main(
        [
            "reply",
            "accept",
            str(invite_file),
        ],
    )

    # Should fail because msmtp failed
    assert result == 1
    mock_popen.assert_called_once()

    # Check what was attempted to be sent to msmtp
    email_content = mock_process.communicate.call_args[0][0]

    # Verify the email was properly formatted even though sending failed
    assert "To: organizer@example.com" in email_content
    assert "From: Testuser <testuser@example.com>" in email_content
    assert "Subject: Re: MSMTP Fail Test - Accepted" in email_content
    assert "This is an automatic reply to your meeting invitation." in email_content
    assert "Status: Accepted" in email_content

    # Verify calendar attachment
    assert "Content-Type: text/calendar" in email_content
    assert 'attachment; filename="reply.ics"' in email_content
    assert "BEGIN:VCALENDAR" in email_content
    assert "METHOD:REPLY" in email_content
    assert "PARTSTAT=ACCEPTED" in email_content
    assert "UID:msmtp-fail@example.com" in email_content


def test_reply_from_email_file(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    """Test replying to calendar invite embedded in email."""
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
UID:email-embedded@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Email Embedded Meeting
ORGANIZER:mailto:organizer@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:recipient@example.com
END:VEVENT
END:VCALENDAR
--boundary--"""

    email_file = tmp_path / "meeting.eml"
    email_file.write_text(email_content)

    result = main(
        [
            "reply",
            "accept",
            "--dry-run",
            str(email_file),
        ],
    )

    assert result == 0

    captured = capsys.readouterr()
    assert "Would send reply to: organizer@example.com" in captured.out
    assert "Status: ACCEPTED" in captured.out


def test_reply_from_stdin(
    tmp_path: Path,  # noqa: ARG001
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Test replying from stdin."""
    invite_content = """From: sender@example.com
To: attendee@example.com
Subject: Meeting Invitation
Content-Type: text/calendar

BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
METHOD:REQUEST
BEGIN:VEVENT
UID:stdin-reply@example.com
DTSTART:20240320T140000Z
DTEND:20240320T150000Z
SUMMARY:Stdin Reply Test
ORGANIZER:mailto:organizer@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:attendee@example.com
END:VEVENT
END:VCALENDAR"""

    monkeypatch.setattr("sys.stdin", io.StringIO(invite_content))

    result = main(
        [
            "reply",
            "decline",
            "--dry-run",
        ],
    )

    assert result == 0

    captured = capsys.readouterr()
    assert "Status: DECLINED" in captured.out
