#!/usr/bin/env python3
"""CLI to create and manage Crab.fit events."""

from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import pathlib
import sys
import urllib.error
import urllib.request
from typing import TYPE_CHECKING, Any, cast
from zoneinfo import ZoneInfo

if TYPE_CHECKING:
    from datetime import date

API_BASE = os.environ.get("CRABFIT_API_URL", "https://api.crab.fit")


def format_time_slot(datetime_obj: dt.datetime) -> str:
    """Convert a datetime to crabfit's HHmm-DDMMYYYY format (UTC)."""
    utc_dt = datetime_obj.astimezone(ZoneInfo("UTC"))
    return (
        f"{utc_dt.hour:02d}{utc_dt.minute:02d}-{utc_dt.day:02d}{utc_dt.month:02d}{utc_dt.year:04d}"
    )


def generate_time_slots(
    dates: list[str],
    start_hour: int,
    end_hour: int,
    timezone: str,
) -> list[str]:
    """Generate time slots for given dates and hour range.

    Args:
        dates: List of dates in YYYY-MM-DD format.
        start_hour: Start hour (0-23).
        end_hour: End hour (1-24, exclusive).
        timezone: IANA timezone string.

    Returns:
        List of time slot strings in HHmm-DDMMYYYY format.

    """
    tz = ZoneInfo(timezone)
    slots = []

    for date_str in dates:
        year, month, day = map(int, date_str.split("-"))

        # Generate hours, handling overnight ranges
        hours: list[int]
        if start_hour < end_hour:
            hours = list(range(start_hour, end_hour))
        else:
            hours = list(range(start_hour, 24)) + list(range(end_hour))

        for hour in hours:
            datetime_obj = dt.datetime(year, month, day, hour, 0, tzinfo=tz)
            slots.append(format_time_slot(datetime_obj))

    return slots


def api_request(
    method: str,
    endpoint: str,
    data: dict[str, Any] | None = None,
) -> dict[str, Any] | list[dict[str, Any]]:
    """Make an API request to crab.fit.

    Args:
        method: HTTP method (GET, POST, PATCH).
        endpoint: API endpoint path.
        data: Optional JSON payload.

    Returns:
        Parsed JSON response.

    """
    url = f"{API_BASE}{endpoint}"
    if not url.startswith(("https://", "http://")):
        msg = f"Invalid URL scheme: {url}"
        raise ValueError(msg)

    headers = {"Content-Type": "application/json", "Accept": "application/json"}

    request_data = json.dumps(data).encode() if data else None
    req = urllib.request.Request(  # noqa: S310
        url,
        data=request_data,
        headers=headers,
        method=method,
    )

    try:
        with urllib.request.urlopen(req) as response:  # noqa: S310
            result: dict[str, Any] | list[dict[str, Any]] = json.loads(response.read().decode())
            return result
    except urllib.error.HTTPError as e:
        error_body = e.read().decode() if e.fp else ""
        print(f"API Error {e.code}: {error_body}", file=sys.stderr)
        sys.exit(1)


def create_event(
    name: str | None,
    dates: list[str],
    start_hour: int,
    end_hour: int,
    timezone: str,
) -> dict[str, Any]:
    """Create a new crabfit event.

    Args:
        name: Event name (optional, auto-generated if None).
        dates: List of dates in YYYY-MM-DD format.
        start_hour: Start hour (0-23).
        end_hour: End hour (1-24, exclusive).
        timezone: IANA timezone string.

    Returns:
        Created event data.

    """
    times = generate_time_slots(dates, start_hour, end_hour, timezone)

    payload: dict[str, Any] = {
        "times": times,
        "timezone": timezone,
    }
    if name:
        payload["name"] = name

    return cast("dict[str, Any]", api_request("POST", "/event", payload))


def get_event(event_id: str) -> dict[str, Any]:
    """Get event details.

    Args:
        event_id: The event ID.

    Returns:
        Event data.

    """
    return cast("dict[str, Any]", api_request("GET", f"/event/{event_id}"))


def get_people(event_id: str) -> list[dict[str, Any]]:
    """Get all people and their availability for an event.

    Args:
        event_id: The event ID.

    Returns:
        List of person data.

    """
    return cast("list[dict[str, Any]]", api_request("GET", f"/event/{event_id}/people"))


def login_or_create_person(event_id: str, name: str) -> dict[str, Any]:
    """Login or create a person for an event.

    Args:
        event_id: The event ID.
        name: Person's name.

    Returns:
        Person data.

    """
    return cast("dict[str, Any]", api_request("GET", f"/event/{event_id}/people/{name}"))


def update_availability(
    event_id: str,
    name: str,
    availability: list[str],
) -> dict[str, Any]:
    """Update a person's availability for an event.

    Args:
        event_id: The event ID.
        name: Person's name.
        availability: List of time slots the person is available.

    Returns:
        Updated person data.

    """
    return cast(
        "dict[str, Any]",
        api_request("PATCH", f"/event/{event_id}/people/{name}", {"availability": availability}),
    )


def parse_date_range(date_range: str) -> list[str]:
    """Parse a date range string into a list of dates.

    Formats:
        - Single date: 2026-01-20
        - Range: 2026-01-20:2026-01-25
        - Comma-separated: 2026-01-20,2026-01-22,2026-01-24
        - Relative: +0 (today), +1 (tomorrow), +0:+6 (next 7 days)

    Args:
        date_range: Date range specification string.

    Returns:
        List of dates in YYYY-MM-DD format.

    """
    dates = []
    today = dt.datetime.now(tz=dt.UTC).date()

    for part in date_range.split(","):
        stripped_part = part.strip()
        if ":" in stripped_part:
            start_str, end_str = stripped_part.split(":")
            start_date = _parse_single_date(start_str.strip(), today)
            end_date = _parse_single_date(end_str.strip(), today)
            current = start_date
            while current <= end_date:
                dates.append(current.strftime("%Y-%m-%d"))
                current += dt.timedelta(days=1)
        else:
            dates.append(_parse_single_date(stripped_part, today).strftime("%Y-%m-%d"))

    return dates


def _parse_single_date(date_str: str, today: date) -> date:
    """Parse a single date string.

    Args:
        date_str: Date string (YYYY-MM-DD or +N for relative).
        today: Reference date for relative dates.

    Returns:
        Parsed date.

    """
    if date_str.startswith("+"):
        days = int(date_str[1:])
        return today + dt.timedelta(days=days)
    return dt.datetime.strptime(date_str, "%Y-%m-%d").replace(tzinfo=dt.UTC).date()


def cmd_create(args: argparse.Namespace) -> None:
    """Handle the create command.

    Args:
        args: Parsed command-line arguments.

    """
    dates = parse_date_range(args.dates)
    timezone: str = args.timezone

    event = create_event(
        name=args.name,
        dates=dates,
        start_hour=args.start,
        end_hour=args.end,
        timezone=timezone,
    )

    print(f"Created event: {event['name']}")
    print(f"URL: https://crab.fit/{event['id']}")
    print(f"Dates: {', '.join(dates)}")
    print(f"Time: {args.start:02d}:00 - {args.end:02d}:00 ({timezone})")

    if args.json:
        print(json.dumps(event, indent=2))


def cmd_respond(args: argparse.Namespace) -> None:
    """Handle the respond command.

    Args:
        args: Parsed command-line arguments.

    """
    event = get_event(args.event_id)
    event_times: list[str] = event["times"]

    # Login/create the person
    login_or_create_person(args.event_id, args.name)

    # Determine availability
    if args.all:
        availability = event_times
    elif args.slots:
        availability = args.slots
    else:
        # Default: mark available for all slots
        availability = event_times

    # Update availability
    result = update_availability(args.event_id, args.name, availability)

    print(f"Updated availability for {result['name']}")
    print(f"Event: {event['name']}")
    print(f"Available for {len(result['availability'])} / {len(event_times)} slots")

    if args.json:
        print(json.dumps(result, indent=2))


def parse_time_slot(slot: str, timezone: str) -> dt.datetime:
    """Parse a crabfit time slot string to datetime.

    Args:
        slot: Time slot in HHmm-DDMMYYYY format.
        timezone: Target timezone for display.

    Returns:
        Datetime in the specified timezone.

    """
    hour = int(slot[0:2])
    minute = int(slot[2:4])
    day = int(slot[5:7])
    month = int(slot[7:9])
    year = int(slot[9:13])

    utc_dt = dt.datetime(year, month, day, hour, minute, tzinfo=dt.UTC)
    return utc_dt.astimezone(ZoneInfo(timezone))


def format_slot_time(datetime_obj: dt.datetime) -> str:
    """Format a datetime as a human-readable time slot.

    Args:
        datetime_obj: The datetime to format.

    Returns:
        Formatted string like "Mon Jan 19 10:00-11:00".

    """
    end_time = datetime_obj + dt.timedelta(hours=1)
    return (
        f"{datetime_obj.strftime('%a %b %d')} "
        f"{datetime_obj.strftime('%H:%M')}-{end_time.strftime('%H:%M')}"
    )


def _build_availability_map(
    event_times: list[str],
    people: list[dict[str, Any]],
) -> dict[str, list[str]]:
    """Build a map of time slots to available people.

    Args:
        event_times: List of all event time slots.
        people: List of people with their availability.

    Returns:
        Map of slot -> list of names available.

    """
    availability_map: dict[str, list[str]] = {slot: [] for slot in event_times}
    for person in people:
        for slot in person["availability"]:
            if slot in availability_map:
                availability_map[slot].append(person["name"])
    return availability_map


def _group_slots_by_count(
    availability_map: dict[str, list[str]],
) -> dict[int, list[str]]:
    """Group time slots by number of people available.

    Args:
        availability_map: Map of slot -> list of names.

    Returns:
        Map of count -> list of slots with that count.

    """
    slots_by_count: dict[int, list[str]] = {}
    for slot, available_names in availability_map.items():
        count = len(available_names)
        if count not in slots_by_count:
            slots_by_count[count] = []
        slots_by_count[count].append(slot)
    return slots_by_count


def _group_consecutive_slots(slots: list[str], timezone: str) -> list[list[str]]:
    """Group consecutive time slots together.

    Args:
        slots: Sorted list of slot strings.
        timezone: Timezone for parsing.

    Returns:
        List of groups, where each group is consecutive slots.

    """
    groups: list[list[str]] = []
    current_group: list[str] = []

    for slot in slots:
        if not current_group:
            current_group = [slot]
            continue

        prev_dt = parse_time_slot(current_group[-1], timezone)
        curr_dt = parse_time_slot(slot, timezone)
        if curr_dt - prev_dt == dt.timedelta(hours=1):
            current_group.append(slot)
        else:
            groups.append(current_group)
            current_group = [slot]

    if current_group:
        groups.append(current_group)

    return groups


def cmd_show(args: argparse.Namespace) -> None:
    """Handle the show command.

    Args:
        args: Parsed command-line arguments.

    """
    event = get_event(args.event_id)
    people = get_people(args.event_id)
    timezone: str = event["timezone"]
    event_times: list[str] = event["times"]

    print(f"Event: {event['name']}")
    print(f"URL: https://crab.fit/{event['id']}")
    print(f"Timezone: {timezone}")

    if not people:
        print("\nNo responses yet.")
        if args.json:
            print(json.dumps({"event": event, "people": people}, indent=2))
        return

    availability_map = _build_availability_map(event_times, people)
    num_people = len(people)
    names = [p["name"] for p in people]

    print(f"Participants ({num_people}): {', '.join(names)}")
    print("\n" + "=" * 50)

    slots_by_count = _group_slots_by_count(availability_map)

    for count in sorted(slots_by_count.keys(), reverse=True):
        if count == 0:
            continue

        slots = sorted(slots_by_count[count])
        if count == num_people:
            label = f"All {num_people} available"
        else:
            label = f"{count}/{num_people} available"
        print(f"\n{label}:")

        for group in _group_consecutive_slots(slots, timezone):
            _print_slot_group(group, timezone, availability_map, num_people)

    if args.json:
        print(json.dumps({"event": event, "people": people}, indent=2))


def _print_slot_group(
    slots: list[str],
    timezone: str,
    availability_map: dict[str, list[str]],
    num_people: int,
) -> None:
    """Print a group of consecutive time slots.

    Args:
        slots: List of consecutive slot strings.
        timezone: Timezone for display.
        availability_map: Map of slot to available names.
        num_people: Total number of participants.

    """
    start_dt = parse_time_slot(slots[0], timezone)
    end_dt = parse_time_slot(slots[-1], timezone) + dt.timedelta(hours=1)

    time_str = (
        f"{start_dt.strftime('%a %b %d')} {start_dt.strftime('%H:%M')}-{end_dt.strftime('%H:%M')}"
    )

    available_names = availability_map[slots[0]]
    count = len(available_names)

    if count == num_people:
        print(f"  ✓ {time_str}")
    else:
        # Get actual missing names by checking who's NOT in available_names
        all_names_set: set[str] = set()
        for names_list in availability_map.values():
            all_names_set.update(names_list)
        missing_names = [n for n in all_names_set if n not in available_names]
        print(f"  • {time_str}  (missing: {', '.join(missing_names)})")


def _get_system_timezone() -> str:
    """Get the system timezone name.

    Returns:
        IANA timezone string, defaults to UTC if detection fails.

    """
    # Check TZ env var first
    tz_env = os.environ.get("TZ")
    if tz_env:
        return tz_env

    # Try /etc/localtime symlink (Linux)
    localtime = pathlib.Path("/etc/localtime")
    if localtime.is_symlink():
        target = str(localtime.resolve())
        # Extract timezone from path like /usr/share/zoneinfo/Europe/Berlin
        if "zoneinfo/" in target:
            return target.split("zoneinfo/")[-1]

    return "UTC"


def main() -> None:
    """CLI entry point."""
    parser = argparse.ArgumentParser(
        description="CLI for creating and managing Crab.fit events",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Create an event for next 3 days, 9am-5pm
  %(prog)s create --dates +0:+2 --start 9 --end 17

  # Create a named event for specific dates
  %(prog)s create --name "Team Meeting" --dates 2026-01-20,2026-01-22

  # Create an event with a specific timezone
  %(prog)s create --dates +0:+6 --timezone America/New_York

  # Show event details
  %(prog)s show my-event-123456
""",
    )

    subparsers = parser.add_subparsers(dest="command", required=True)

    # Create command
    create_parser = subparsers.add_parser("create", help="Create a new event")
    create_parser.add_argument(
        "--name",
        "-n",
        help="Event name (auto-generated if not provided)",
    )
    create_parser.add_argument(
        "--dates",
        "-d",
        required=True,
        help="Dates: YYYY-MM-DD, range (date1:date2), relative (+N), or comma-separated",
    )
    create_parser.add_argument(
        "--start",
        "-s",
        type=int,
        default=9,
        help="Start hour (0-23, default: 9)",
    )
    create_parser.add_argument(
        "--end",
        "-e",
        type=int,
        default=17,
        help="End hour (1-24, default: 17)",
    )
    create_parser.add_argument(
        "--timezone",
        "-t",
        default=_get_system_timezone(),
        help="Timezone (default: system timezone)",
    )
    create_parser.add_argument(
        "--json",
        action="store_true",
        help="Output full JSON response",
    )
    create_parser.set_defaults(func=cmd_create)

    # Respond command
    respond_parser = subparsers.add_parser("respond", help="Add your availability to an event")
    respond_parser.add_argument("event_id", help="Event ID or URL")
    respond_parser.add_argument(
        "--name",
        "-n",
        required=True,
        help="Your name",
    )
    respond_parser.add_argument(
        "--all",
        "-a",
        action="store_true",
        help="Mark available for all time slots",
    )
    respond_parser.add_argument(
        "--slots",
        nargs="+",
        help="Specific time slots (in HHmm-DDMMYYYY format)",
    )
    respond_parser.add_argument(
        "--json",
        action="store_true",
        help="Output full JSON response",
    )
    respond_parser.set_defaults(func=cmd_respond)

    # Show command
    show_parser = subparsers.add_parser("show", help="Show event details")
    show_parser.add_argument("event_id", help="Event ID or URL")
    show_parser.add_argument(
        "--json",
        action="store_true",
        help="Output full JSON response",
    )
    show_parser.set_defaults(func=cmd_show)

    args = parser.parse_args()

    # Extract event ID from URL if needed
    if hasattr(args, "event_id") and "crab.fit/" in args.event_id:
        args.event_id = args.event_id.split("crab.fit/")[-1].split("?")[0]

    args.func(args)


if __name__ == "__main__":
    main()
