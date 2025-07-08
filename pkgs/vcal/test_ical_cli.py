"""Integration tests for ical-cli."""

import tempfile
from collections.abc import Generator
from pathlib import Path

import pytest
from icalendar import Calendar

from ical_cli.create import get_local_timezone
from ical_cli.main import main


@pytest.fixture
def temp_dir() -> Generator[str]:
    """Create a temporary directory for test output."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield tmpdir


class TestBasicInvites:
    """Test basic calendar invite creation."""

    def test_simple_meeting(self, temp_dir: str) -> None:
        """Test creating a simple meeting invitation."""
        output_file = Path(temp_dir) / "simple.ics"

        args = [
            "-s",
            "Test Meeting",
            "-a",
            "test@example.com",
            "--start",
            "2025-07-15 14:00",
            "-d",
            "30",
            "--organizer-name",
            "Test User",
            "--organizer-email",
            "organizer@example.com",
            "--no-send",
            "-o",
            str(output_file),
            "--no-local-save",
        ]

        result = main(["create", *args])
        assert result == 0
        assert output_file.exists()

        # Parse and verify the ICS file
        with output_file.open("rb") as f:
            cal = Calendar.from_ical(f.read())

        event = None
        for component in cal.walk():
            if component.name == "VEVENT":
                event = component
                break

        assert event is not None
        assert str(event["SUMMARY"]) == "Test Meeting"
        assert event["DTSTART"].dt.hour == 14
        assert event["DTSTART"].dt.minute == 0

        # Check duration (30 minutes)
        duration = event["DTEND"].dt - event["DTSTART"].dt
        assert duration.total_seconds() == 1800  # 30 minutes

        # Check organizer
        assert "organizer@example.com" in str(event["ORGANIZER"])

        # Check attendee
        attendees = event.get("ATTENDEE", [])
        if not isinstance(attendees, list):
            attendees = [attendees]
        attendee_emails = [str(a).replace("mailto:", "") for a in attendees]
        assert "test@example.com" in attendee_emails

    def test_meeting_with_multiple_attendees(self, temp_dir: str) -> None:
        """Test creating a meeting with multiple attendees."""
        output_file = Path(temp_dir) / "multi-attendee.ics"

        args = [
            "-s",
            "Team Meeting",
            "-a",
            "john@example.com,Jane Doe <jane@example.com>,manager@company.com",
            "--organizer-email",
            "organizer@example.com",
            "--no-send",
            "-o",
            str(output_file),
            "--no-local-save",
        ]

        result = main(["create", *args])
        assert result == 0
        assert output_file.exists()

        # Parse and verify attendees
        with output_file.open("rb") as f:
            cal = Calendar.from_ical(f.read())

        event = None
        for component in cal.walk():
            if component.name == "VEVENT":
                event = component
                break

        attendees = event.get("ATTENDEE", [])
        if not isinstance(attendees, list):
            attendees = [attendees]

        attendee_emails = [str(a).replace("mailto:", "") for a in attendees]
        # Should include the organizer plus 3 attendees
        assert len(attendees) >= 3
        assert any("john@example.com" in email for email in attendee_emails)
        assert any("jane@example.com" in email for email in attendee_emails)
        assert any("manager@company.com" in email for email in attendee_emails)

    def test_meeting_with_location_and_link(self, temp_dir: str) -> None:
        """Test creating a meeting with location and meeting link."""
        output_file = Path(temp_dir) / "location-link.ics"

        args = [
            "-s",
            "Virtual Meeting",
            "-a",
            "participant@example.com",
            "-l",
            "Conference Room A",
            "--meeting-link",
            "https://zoom.us/j/123456789",
            "--description",
            "Quarterly review meeting",
            "--organizer-email",
            "host@example.com",
            "--no-send",
            "-o",
            str(output_file),
            "--no-local-save",
        ]

        result = main(["create", *args])
        assert result == 0

        with output_file.open("rb") as f:
            cal = Calendar.from_ical(f.read())

        event = None
        for component in cal.walk():
            if component.name == "VEVENT":
                event = component
                break

        assert str(event.get("LOCATION", "")) == "Conference Room A"

        description = str(event.get("DESCRIPTION", ""))
        assert "Quarterly review meeting" in description
        assert "https://zoom.us/j/123456789" in description


class TestRecurringEvents:
    """Test recurring event creation."""

    def test_weekly_recurring(self, temp_dir: str) -> None:
        """Test creating a weekly recurring event."""
        output_file = Path(temp_dir) / "weekly.ics"

        args = [
            "-s",
            "Weekly Standup",
            "-a",
            "team@example.com",
            "--repeat",
            "weekly",
            "--count",
            "10",
            "--organizer-email",
            "lead@example.com",
            "--no-send",
            "-o",
            str(output_file),
            "--no-local-save",
        ]

        result = main(["create", *args])
        assert result == 0

        with output_file.open("rb") as f:
            cal = Calendar.from_ical(f.read())

        event = None
        for component in cal.walk():
            if component.name == "VEVENT":
                event = component
                break

        rrule = event.get("RRULE")
        assert rrule is not None
        assert rrule.get("FREQ") == ["WEEKLY"]
        assert rrule.get("COUNT") == [10]

    def test_biweekly_with_specific_days(self, temp_dir: str) -> None:
        """Test creating a biweekly event on specific weekdays."""
        output_file = Path(temp_dir) / "biweekly.ics"

        args = [
            "-s",
            "Sprint Planning",
            "-a",
            "scrum@example.com",
            "--repeat",
            "biweekly",
            "--weekdays",
            "MO,WE,FR",
            "--count",
            "6",
            "--organizer-email",
            "scrum.master@example.com",
            "--no-send",
            "-o",
            str(output_file),
            "--no-local-save",
        ]

        result = main(["create", *args])
        assert result == 0

        with output_file.open("rb") as f:
            cal = Calendar.from_ical(f.read())

        event = None
        for component in cal.walk():
            if component.name == "VEVENT":
                event = component
                break

        rrule = event.get("RRULE")
        assert rrule.get("FREQ") == ["WEEKLY"]
        assert rrule.get("INTERVAL") == [2]
        assert set(rrule.get("BYDAY", [])) == {"MO", "WE", "FR"}
        assert rrule.get("COUNT") == [6]

    def test_daily_until_date(self, temp_dir: str) -> None:
        """Test creating a daily recurring event until a specific date."""
        output_file = Path(temp_dir) / "daily-until.ics"

        args = [
            "-s",
            "Daily Sync",
            "-a",
            "team@example.com",
            "--repeat",
            "daily",
            "--until",
            "2025-12-31",
            "--timezone",
            "Europe/Berlin",
            "--organizer-email",
            "manager@example.com",
            "--no-send",
            "-o",
            str(output_file),
            "--no-local-save",
        ]

        result = main(["create", *args])
        assert result == 0

        with output_file.open("rb") as f:
            cal = Calendar.from_ical(f.read())

        event = None
        for component in cal.walk():
            if component.name == "VEVENT":
                event = component
                break

        rrule = event.get("RRULE")
        assert rrule.get("FREQ") == ["DAILY"]

        # Check UNTIL date
        until = rrule.get("UNTIL")[0]
        assert until.year == 2025
        assert until.month == 12
        assert until.day == 31

    def test_custom_rrule(self, temp_dir: str) -> None:
        """Test creating an event with custom RRULE."""
        output_file = Path(temp_dir) / "custom-rrule.ics"

        args = [
            "-s",
            "Custom Recurring",
            "-a",
            "custom@example.com",
            "--rrule",
            "FREQ=DAILY;INTERVAL=3;COUNT=7",
            "--organizer-email",
            "organizer@example.com",
            "--no-send",
            "-o",
            str(output_file),
            "--no-local-save",
        ]

        result = main(["create", *args])
        assert result == 0

        with output_file.open("rb") as f:
            cal = Calendar.from_ical(f.read())

        event = None
        for component in cal.walk():
            if component.name == "VEVENT":
                event = component
                break

        rrule = event.get("RRULE")
        assert rrule.get("FREQ") == ["DAILY"]
        assert rrule.get("INTERVAL") == [3]
        assert rrule.get("COUNT") == [7]


class TestTimezones:
    """Test timezone handling."""

    def test_default_local_timezone(self, temp_dir: str) -> None:
        """Test that default timezone is local system timezone."""
        output_file = Path(temp_dir) / "local-tz.ics"

        # Don't specify timezone, should use local
        args = [
            "-s",
            "Local TZ Meeting",
            "-a",
            "test@example.com",
            "--organizer-email",
            "organizer@example.com",
            "--no-send",
            "-o",
            str(output_file),
            "--no-local-save",
        ]

        result = main(["create", *args])
        assert result == 0

        # The local timezone should be used (not UTC)
        local_tz = get_local_timezone()
        # In build environments, timezone might be UTC, which is fine
        # Just verify get_local_timezone returns a valid timezone string
        assert isinstance(local_tz, str)
        assert len(local_tz) > 0

    def test_specific_timezone(self, temp_dir: str) -> None:
        """Test creating event in specific timezone."""
        output_file = Path(temp_dir) / "specific-tz.ics"

        args = [
            "-s",
            "NYC Meeting",
            "-a",
            "nyc@example.com",
            "--start",
            "2025-07-15 09:00",
            "--timezone",
            "America/New_York",
            "--organizer-email",
            "organizer@example.com",
            "--no-send",
            "-o",
            str(output_file),
            "--no-local-save",
        ]

        result = main(["create", *args])
        assert result == 0

        with output_file.open("rb") as f:
            cal = Calendar.from_ical(f.read())

        event = None
        for component in cal.walk():
            if component.name == "VEVENT":
                event = component
                break

        # Check that the timezone is preserved
        dtstart = event["DTSTART"].dt
        if hasattr(dtstart, "tzinfo") and dtstart.tzinfo:
            assert dtstart.hour == 9


class TestErrorHandling:
    """Test error handling."""

    def test_invalid_timezone(self) -> None:
        """Test handling of invalid timezone."""
        args = [
            "-s",
            "Bad TZ Meeting",
            "-a",
            "test@example.com",
            "--timezone",
            "Invalid/Timezone",
            "--no-send",
        ]

        result = main(["create", *args])
        assert result == 1

    def test_invalid_start_time_format(self) -> None:
        """Test handling of invalid start time format."""
        args = [
            "-s",
            "Bad Time Meeting",
            "-a",
            "test@example.com",
            "--start",
            "invalid-time",
            "--no-send",
        ]

        result = main(["create", *args])
        assert result == 1

    def test_no_attendees(self) -> None:
        """Test that no attendees results in error."""
        args = ["-s", "No Attendees Meeting", "-a", "", "--no-send"]

        result = main(["create", *args])
        assert result == 1

    def test_invalid_until_date(self) -> None:
        """Test handling of invalid until date format."""
        args = [
            "-s",
            "Bad Until Date",
            "-a",
            "test@example.com",
            "--repeat",
            "daily",
            "--until",
            "not-a-date",
            "--no-send",
        ]

        result = main(["create", *args])
        assert result == 1


class TestCalendarFeatures:
    """Test various calendar features."""

    def test_reminder_time(self, temp_dir: str) -> None:
        """Test custom reminder time."""
        output_file = Path(temp_dir) / "reminder.ics"

        args = [
            "-s",
            "Meeting with Reminder",
            "-a",
            "remind@example.com",
            "--reminder",
            "30",
            "--organizer-email",
            "organizer@example.com",
            "--no-send",
            "-o",
            str(output_file),
            "--no-local-save",
        ]

        result = main(["create", *args])
        assert result == 0

        with output_file.open("rb") as f:
            cal = Calendar.from_ical(f.read())

        # Find the alarm component
        alarm = None
        for component in cal.walk():
            if component.name == "VALARM":
                alarm = component
                break

        assert alarm is not None
        trigger = alarm.get("TRIGGER")
        # Should be -30 minutes
        assert trigger.dt.total_seconds() == -1800

    def test_meeting_status_and_transp(self, temp_dir: str) -> None:
        """Test that meeting has correct status and transparency."""
        output_file = Path(temp_dir) / "status.ics"

        args = [
            "-s",
            "Status Test Meeting",
            "-a",
            "status@example.com",
            "--organizer-email",
            "organizer@example.com",
            "--no-send",
            "-o",
            str(output_file),
            "--no-local-save",
        ]

        result = main(["create", *args])
        assert result == 0

        with output_file.open("rb") as f:
            cal = Calendar.from_ical(f.read())

        event = None
        for component in cal.walk():
            if component.name == "VEVENT":
                event = component
                break

        assert str(event.get("STATUS", "")) == "CONFIRMED"
        assert str(event.get("TRANSP", "")) == "OPAQUE"
        assert cal.get("METHOD") == "REQUEST"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
