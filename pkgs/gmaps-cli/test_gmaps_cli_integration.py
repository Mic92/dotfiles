#!/usr/bin/env python3
"""Integration tests for gmaps-cli using pytest."""

import io
import sys
from contextlib import redirect_stdout
from pathlib import Path

import pytest

# Add the current directory to Python path so we can import gmaps_cli
sys.path.insert(0, str(Path(__file__).parent))

from gmaps_cli import main


@pytest.fixture
def config_exists() -> bool:
    """Check if API key is configured."""
    config_file = Path.home() / ".config" / "gmaps-cli" / "config.json"
    if not config_file.exists():
        pytest.skip("No API key configured. Run 'gmaps-cli setup' first.")
    return True


def capture_cli_output(args: list[str]) -> str:
    """Helper to capture CLI output."""
    output = io.StringIO()
    try:
        with redirect_stdout(output):
            main(args)
    except SystemExit:
        pass  # Ignore exit codes
    return output.getvalue()


def test_munich_to_berlin_route(config_exists: bool) -> None:
    """Test finding route from Munich to Berlin using the CLI."""
    # Call the CLI with route command
    result = capture_cli_output(
        ["route", "Munich, Germany", "Berlin, Germany", "-m", "driving"]
    )

    # Verify the output contains expected elements
    assert "Munich" in result or "München" in result
    assert "Berlin" in result
    assert "Distance:" in result
    assert "Duration:" in result
    assert "View on Google Maps:" in result
    assert "Directions:" in result

    # Check that distance is reasonable (500-700 km)
    lines = result.split("\n")
    distance_found = False
    for line in lines:
        if "Distance:" in line and "km" in line:
            distance_str = line.split(":")[1].strip()
            distance_val = float(distance_str.replace(" km", ""))
            assert 500 <= distance_val <= 700, f"Unexpected distance: {distance_val} km"
            distance_found = True
            break

    assert distance_found, "Distance not found in output"

    # Check for Google Maps URL
    url_found = False
    for line in lines:
        if "View on Google Maps:" in line:
            url = line.split(":", 1)[1].strip()
            assert url.startswith("https://www.google.com/maps/dir/")
            assert "Munich" in url or "M%C3%BCnchen" in url
            assert "Berlin" in url
            url_found = True
            break

    assert url_found, "Google Maps URL not found"


def test_transit_route(config_exists: bool) -> None:
    """Test Munich to Berlin using transit (train)."""
    result = capture_cli_output(
        ["route", "Munich Hauptbahnhof", "Berlin Hauptbahnhof", "-m", "transit"]
    )

    # Transit routes should show transit-specific information
    assert "transit" in result.lower() or "Transit" in result

    # Should have note about scheduled times
    assert "Note: Transit times shown are scheduled times" in result


def test_search_place(config_exists: bool) -> None:
    """Test searching for a specific place."""
    result = capture_cli_output(["search", "Marienplatz Munich"])

    # Should find Marienplatz
    assert "Marienplatz" in result or "marienplatz" in result.lower()
    assert "View on Google Maps:" in result

    # Check for coordinates
    lines = result.split("\n")
    found_coords = False
    for line in lines:
        if (
            ", " in line
            and line.replace(".", "")
            .replace(",", "")
            .replace(" ", "")
            .replace("-", "")
            .isdigit()
        ):
            # This looks like coordinates
            parts = line.split(", ")
            if len(parts) == 2:
                try:
                    lat = float(parts[0])
                    lng = float(parts[1])
                    # Munich is around 48.1°N, 11.5°E
                    if 47 < lat < 49 and 10 < lng < 13:
                        found_coords = True
                        break
                except ValueError:
                    pass

    assert found_coords, "Coordinates not found in output"


def test_nearby_restaurants(config_exists: bool) -> None:
    """Test finding restaurants near a location."""
    result = capture_cli_output(
        ["nearby", "restaurants", "-l", "Marienplatz Munich", "-n", "3"]
    )

    # Should find some restaurants
    lines = result.split("\n")
    restaurant_count = 0

    for line in lines:
        if line.strip().startswith(("1.", "2.", "3.")):
            restaurant_count += 1

    assert restaurant_count > 0, "No restaurants found"
    assert "Maps:" in result, "No Maps links found"


def test_help_command() -> None:
    """Test the help command."""
    result = capture_cli_output(["--help"])

    assert "usage:" in result.lower()
    assert "route" in result
    assert "search" in result
    assert "nearby" in result
    assert "setup" in result


def test_no_command() -> None:
    """Test behavior when no command is given."""
    result = capture_cli_output([])

    # Should show help when no command given
    assert "usage:" in result.lower()


@pytest.mark.parametrize(
    ("origin", "destination", "mode"),
    [
        ("Munich, Germany", "Berlin, Germany", "driving"),
        ("Hamburg, Germany", "Frankfurt, Germany", "driving"),
        ("Munich Hauptbahnhof", "Berlin Hauptbahnhof", "transit"),
    ],
)
def test_various_routes(
    config_exists: bool, origin: str, destination: str, mode: str
) -> None:
    """Test various routes with different parameters."""
    result = capture_cli_output(["route", origin, destination, "-m", mode])

    assert "Distance:" in result
    assert "Duration:" in result
    assert "View on Google Maps:" in result


@pytest.mark.parametrize(
    ("query", "expected_in_result"),
    [
        ("Brandenburger Tor Berlin", ["Brandenburg", "Berlin"]),
        ("Neuschwanstein Castle", ["Neuschwanstein"]),
        ("Cologne Cathedral", ["Cathedral", "Cologne"]),
    ],
)
def test_search_various_places(
    config_exists: bool, query: str, expected_in_result: list[str]
) -> None:
    """Test searching for various German landmarks."""
    result = capture_cli_output(["search", query])

    # Check that at least one of the expected terms is in the result
    assert any(term in result for term in expected_in_result)
    assert "View on Google Maps:" in result
