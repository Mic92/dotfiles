#!/usr/bin/env python3
import argparse
import json
import subprocess
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any

# Config directory
CONFIG_DIR = Path.home() / ".config" / "gmaps-cli"
CONFIG_FILE = CONFIG_DIR / "config.json"


def parse_datetime(dt_str: str) -> str:
    """Parse datetime string and convert to ISO 8601 format for Google API"""
    # Try parsing common formats
    formats = [
        "%Y-%m-%d %H:%M",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%dT%H:%M:%SZ",
        "%Y-%m-%dT%H:%M:%S%z",
    ]

    for fmt in formats:
        try:
            # Parse as timezone-aware datetime
            if "%z" in fmt or "Z" in fmt:
                dt = datetime.strptime(dt_str, fmt)  # noqa: DTZ007
            else:
                # For formats without timezone, create timezone-aware datetime assuming local time
                dt = datetime.strptime(dt_str, fmt).astimezone()
            return dt.isoformat()
        except ValueError:
            continue

    # If no format matched, return as-is (might already be in correct format)
    return dt_str


def search_place(api_key: str, query: str) -> dict[str, Any] | None:
    """Search for a place using Places API (New)"""
    endpoint = "https://places.googleapis.com/v1/places:searchText"

    headers = {
        "Content-Type": "application/json",
        "X-Goog-Api-Key": api_key,
        "X-Goog-FieldMask": "places.displayName,places.formattedAddress,places.location,places.rating,places.userRatingCount,places.id",
    }

    body = {"textQuery": query, "maxResultCount": 1}

    try:
        request = urllib.request.Request(  # noqa: S310
            endpoint,
            data=json.dumps(body).encode("utf-8"),
            headers=headers,
            method="POST",
        )
        with urllib.request.urlopen(request, timeout=10) as response:  # noqa: S310
            data = json.loads(response.read())
    except urllib.error.HTTPError as e:
        error_data = e.read().decode("utf-8")
        print(f"HTTP Error {e.code}: {e.reason}")
        try:
            error_json = json.loads(error_data)
            if "error" in error_json and "message" in error_json["error"]:
                print(f"API Error: {error_json['error']['message']}")
        except json.JSONDecodeError:
            print(f"Response: {error_data}")
        return None
    except (urllib.error.URLError, json.JSONDecodeError) as e:
        print(f"Error: {e}")
        return None

    if data.get("places"):
        place = data["places"][0]
        result = {
            "name": place.get("displayName", {}).get("text", "Unknown"),
            "address": place.get("formattedAddress", ""),
            "place_id": place.get("id", ""),
            "rating": place.get("rating"),
            "ratings_total": place.get("userRatingCount", 0),
        }
        # Add location if available
        if "location" in place:
            result["lat"] = place["location"].get("latitude")
            result["lng"] = place["location"].get("longitude")
        else:
            result["lat"] = None
            result["lng"] = None
        return result
    return None


def search_places_nearby(
    api_key: str, query: str, location: str | None = None
) -> list[dict[str, Any]]:
    """Search for multiple places nearby using Places API (New)"""
    endpoint = "https://places.googleapis.com/v1/places:searchText"

    headers = {
        "Content-Type": "application/json",
        "X-Goog-Api-Key": api_key,
        "X-Goog-FieldMask": "places.displayName,places.formattedAddress,places.location,places.rating,places.priceLevel,places.id",
    }

    body = {"textQuery": query, "maxResultCount": 10}

    # Add location bias if provided
    if location:
        lat, lng = location.split(",")
        body["locationBias"] = {
            "circle": {
                "center": {"latitude": float(lat), "longitude": float(lng)},
                "radius": 5000.0,  # 5km radius
            }
        }

    try:
        request = urllib.request.Request(  # noqa: S310
            endpoint,
            data=json.dumps(body).encode("utf-8"),
            headers=headers,
            method="POST",
        )
        with urllib.request.urlopen(request, timeout=10) as response:  # noqa: S310
            data = json.loads(response.read())
    except (urllib.error.URLError, json.JSONDecodeError) as e:
        print(f"Error: {e}")
        return []

    return [
        {
            "name": place.get("displayName", {}).get("text", "Unknown"),
            "address": place.get("formattedAddress", ""),
            "lat": place.get("location", {}).get("latitude"),
            "lng": place.get("location", {}).get("longitude"),
            "place_id": place.get("id", ""),
            "rating": place.get("rating"),
            "price_level": place.get("priceLevel"),
        }
        for place in data.get("places", [])
    ]


def get_directions(
    api_key: str,
    origin: str,
    destination: str,
    mode: str = "driving",
    departure_time: str | None = None,
    arrival_time: str | None = None,
) -> dict[str, Any] | None:
    """Get directions between two places using Routes API"""
    endpoint = "https://routes.googleapis.com/directions/v2:computeRoutes"

    headers = {
        "Content-Type": "application/json",
        "X-Goog-Api-Key": api_key,
        "X-Goog-FieldMask": "routes.legs.steps.navigationInstruction,routes.legs.steps.localizedValues,routes.legs.localizedValues,routes.legs.polyline,routes.distanceMeters,routes.duration,routes.legs.steps.transitDetails",
    }

    # Map mode to travel mode
    travel_modes = {
        "driving": "DRIVE",
        "walking": "WALK",
        "bicycling": "BICYCLE",
        "transit": "TRANSIT",
    }

    body = {
        "origin": {"address": origin},
        "destination": {"address": destination},
        "travelMode": travel_modes.get(mode, "DRIVE"),
        "computeAlternativeRoutes": False,
        "languageCode": "en-US",
        "units": "METRIC",
    }

    # Add departure or arrival time if specified
    if departure_time:
        body["departureTime"] = departure_time
    elif arrival_time:
        body["arrivalTime"] = arrival_time

    try:
        request = urllib.request.Request(  # noqa: S310
            endpoint,
            data=json.dumps(body).encode("utf-8"),
            headers=headers,
            method="POST",
        )
        with urllib.request.urlopen(request, timeout=10) as response:  # noqa: S310
            data = json.loads(response.read())
    except urllib.error.HTTPError as e:
        error_data = e.read().decode("utf-8")
        print(f"Error: HTTP Error {e.code}: {e.reason}")
        try:
            error_json = json.loads(error_data)
            if "error" in error_json:
                print(f"API Error: {error_json['error']}")
        except json.JSONDecodeError:
            print(f"Response: {error_data}")
        return None
    except (urllib.error.URLError, json.JSONDecodeError) as e:
        print(f"Error: {e}")
        return None

    if data.get("routes") and data["routes"]:
        route = data["routes"][0]
        if route.get("legs"):
            leg = route["legs"][0]
            # Convert meters to human-readable format
            distance_m = route.get("distanceMeters", 0)
            if distance_m >= 1000:
                distance_text = f"{distance_m / 1000:.1f} km"
            else:
                distance_text = f"{distance_m} m"

            # Convert duration to human-readable format
            duration_s = int(route.get("duration", "0s").rstrip("s"))
            hours = duration_s // 3600
            minutes = (duration_s % 3600) // 60
            if hours > 0:
                duration_text = f"{hours} hour{'s' if hours > 1 else ''} {minutes} min{'s' if minutes != 1 else ''}"
            else:
                duration_text = f"{minutes} min{'s' if minutes != 1 else ''}"

            steps = []
            for step in leg.get("steps", []):
                if not step.get("navigationInstruction"):
                    continue

                step_info = {
                    "instruction": step.get("navigationInstruction", {}).get(
                        "instructions", "Continue"
                    ),
                    "distance": step.get("localizedValues", {})
                    .get("distance", {})
                    .get("text", ""),
                    "duration": step.get("localizedValues", {})
                    .get("staticDuration", {})
                    .get("text", ""),
                }

                # Add transit details if available
                if "transitDetails" in step:
                    transit = step["transitDetails"]
                    step_info["transit"] = {}

                    # Transit line info
                    if "transitLine" in transit:
                        line = transit["transitLine"]
                        step_info["transit"]["line_name"] = line.get(
                            "nameShort"
                        ) or line.get("name", "")
                        step_info["transit"]["vehicle"] = line.get("vehicle", {}).get(
                            "type", ""
                        )

                    # Stop details
                    if "stopDetails" in transit:
                        stops = transit["stopDetails"]
                        if "departureStop" in stops:
                            step_info["transit"]["departure_stop"] = stops[
                                "departureStop"
                            ].get("name", "")
                        if "arrivalStop" in stops:
                            step_info["transit"]["arrival_stop"] = stops[
                                "arrivalStop"
                            ].get("name", "")
                        if "stopCount" in stops:
                            step_info["transit"]["stop_count"] = stops["stopCount"]

                    # Time details
                    if "localizedValues" in transit:
                        loc_vals = transit["localizedValues"]
                        if "departureTime" in loc_vals:
                            step_info["transit"]["departure_time"] = (
                                loc_vals["departureTime"]
                                .get("time", {})
                                .get("text", "")
                            )
                        if "arrivalTime" in loc_vals:
                            step_info["transit"]["arrival_time"] = (
                                loc_vals["arrivalTime"].get("time", {}).get("text", "")
                            )

                steps.append(step_info)

            return {
                "distance": distance_text,
                "duration": duration_text,
                "start_address": origin,
                "end_address": destination,
                "steps": steps,
            }
    return None


def get_api_key(config: dict[str, Any]) -> str | None:
    """Get API key from command if configured"""
    if "api_key_command" in config:
        try:
            result = subprocess.run(  # noqa: S602
                config["api_key_command"],
                check=False,
                shell=True,
                capture_output=True,
                text=True,
                timeout=10,
            )
            if result.returncode == 0:
                return result.stdout.strip()
            print(f"Error running API key command: {result.stderr}")
        except subprocess.TimeoutExpired:
            print("API key command timed out")
            return None
        except (OSError, ValueError) as e:
            print(f"Error getting API key: {e}")
            return None
        else:
            return None
    return None


def load_config() -> dict[str, Any]:
    """Load configuration"""
    if CONFIG_FILE.exists():
        with CONFIG_FILE.open() as f:
            config: dict[str, Any] = json.load(f)
            return config
    return {}


def save_config(config: dict[str, Any]) -> None:
    """Save configuration"""
    CONFIG_DIR.mkdir(parents=True, exist_ok=True)
    with CONFIG_FILE.open("w") as f:
        json.dump(config, f, indent=2)


def setup(api_key_command: str) -> None:
    """Setup API key command"""
    config = {"api_key_command": api_key_command}
    save_config(config)

    # Test the command
    api_key = get_api_key(config)
    if api_key:
        print("Setup complete! API key command works.")
    else:
        print(
            "Warning: API key command did not return a key. Please check your command."
        )


def search(query: str) -> None:
    """Search for a specific place"""
    config = load_config()
    if "api_key_command" not in config:
        print("Please run 'gmaps-cli setup --api-key-command YOUR_COMMAND' first")
        return

    api_key = get_api_key(config)
    if not api_key:
        print("Failed to get API key from command")
        return

    place_data = search_place(api_key, query)

    if not place_data:
        print(f"No results found for '{query}'")
        return

    print(f"{place_data['name']}")
    print(f"{place_data['address']}")
    if place_data["lat"] and place_data["lng"]:
        print(f"{place_data['lat']}, {place_data['lng']}")
    if place_data["rating"]:
        print(f"Rating: {place_data['rating']} ({place_data['ratings_total']} reviews)")


def nearby(query: str, location: str | None = None, limit: int = 5) -> None:
    """Search for places nearby"""
    config = load_config()
    if "api_key_command" not in config:
        print("Please run 'gmaps-cli setup --api-key-command YOUR_COMMAND' first")
        return

    api_key = get_api_key(config)
    if not api_key:
        print("Failed to get API key from command")
        return

    # If location is provided and doesn't look like coordinates, search for it first
    if location and not (
        location.count(",") == 1
        and all(
            part.replace(".", "").replace("-", "").isdigit()
            for part in location.split(",")
        )
    ):
        place_data = search_place(api_key, location)
        if place_data and place_data["lat"] and place_data["lng"]:
            location = f"{place_data['lat']},{place_data['lng']}"
            print(f"Searching near: {place_data['name']}, {place_data['address']}\n")
        else:
            print(f"Could not find location: {location}")
            return

    results = search_places_nearby(api_key, query, location)

    if not results:
        print(f"No results found for '{query}'")
        return

    for i, place in enumerate(results[:limit], 1):
        print(f"\n{i}. {place['name']}")
        print(f"   {place['address']}")
        if place["rating"]:
            # Convert price level string to dollar signs
            price_map = {
                "PRICE_LEVEL_FREE": "",
                "PRICE_LEVEL_INEXPENSIVE": "$",
                "PRICE_LEVEL_MODERATE": "$$",
                "PRICE_LEVEL_EXPENSIVE": "$$$",
                "PRICE_LEVEL_VERY_EXPENSIVE": "$$$$",
            }
            price = (
                price_map.get(place["price_level"], "") if place["price_level"] else ""
            )
            print(f"   Rating: {place['rating']} {price}")


def route(
    origin: str,
    destination: str,
    mode: str = "driving",
    departure_time: str | None = None,
    arrival_time: str | None = None,
) -> None:
    """Get directions between two places"""
    config = load_config()
    if "api_key_command" not in config:
        print("Please run 'gmaps-cli setup --api-key-command YOUR_COMMAND' first")
        return

    api_key = get_api_key(config)
    if not api_key:
        print("Failed to get API key from command")
        return

    # Parse datetime arguments if provided
    if departure_time:
        departure_time = parse_datetime(departure_time)
    if arrival_time:
        arrival_time = parse_datetime(arrival_time)

    directions = get_directions(
        api_key, origin, destination, mode, departure_time, arrival_time
    )

    if not directions:
        print(f"No route found from '{origin}' to '{destination}'")
        return

    print(f"Route from {directions['start_address']} to {directions['end_address']}")
    print(f"Distance: {directions['distance']}")
    print(f"Duration: {directions['duration']}")

    # If arrival time was specified, calculate and show departure time
    if arrival_time:
        try:
            # Parse the arrival time and duration to calculate departure
            arrival_dt = datetime.fromisoformat(arrival_time)
            duration_str = directions["duration"]

            # Parse duration (e.g., "51 mins" or "1 hour 45 mins")
            total_minutes = 0
            if "hour" in duration_str:
                parts = duration_str.split()
                for i, part in enumerate(parts):
                    if "hour" in part:
                        total_minutes += int(parts[i - 1]) * 60
                    elif "min" in part and i > 0 and "hour" not in parts[i - 1]:
                        total_minutes += int(parts[i - 1])
            else:
                # Just minutes
                total_minutes = int(duration_str.split()[0])

            # Calculate departure time
            departure_dt = arrival_dt - timedelta(minutes=total_minutes)
            print(
                f"\nTo arrive by {arrival_dt.strftime('%Y-%m-%d %H:%M')}, depart at: {departure_dt.strftime('%Y-%m-%d %H:%M')}"
            )
        except (ValueError, KeyError, AttributeError):
            # Skip if parsing fails
            pass

    print("\nDirections:")

    for i, step in enumerate(directions["steps"], 1):
        print(f"\n{i}. {step['instruction']}")

        # Show transit details if available
        if "transit" in step:
            transit = step["transit"]
            if "line_name" in transit:
                vehicle = transit.get("vehicle", "").replace("_", " ").title()
                print(f"   Take {vehicle}: {transit['line_name']}")
            if "departure_stop" in transit:
                dep_time = (
                    f" at {transit['departure_time']}"
                    if transit.get("departure_time")
                    else ""
                )
                print(f"   From: {transit['departure_stop']}{dep_time}")
            if "arrival_stop" in transit:
                arr_time = (
                    f" at {transit['arrival_time']}"
                    if transit.get("arrival_time")
                    else ""
                )
                print(f"   To: {transit['arrival_stop']}{arr_time}")
            if "stop_count" in transit:
                print(f"   Stops: {transit['stop_count']}")

        print(f"   {step['distance']} - {step['duration']}")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Search for places and get directions using Google Maps API"
    )
    subparsers = parser.add_subparsers(dest="command", help="Available commands")

    # Setup command
    setup_parser = subparsers.add_parser("setup", help="Setup API key command")
    setup_parser.add_argument(
        "--api-key-command",
        required=True,
        help="Command to get Google Maps API key (e.g., 'pass show google-maps-api-key')",
    )

    # Search command
    search_parser = subparsers.add_parser("search", help="Search for a specific place")
    search_parser.add_argument("query", help="Place to search for")

    # Nearby command
    nearby_parser = subparsers.add_parser("nearby", help="Search for places nearby")
    nearby_parser.add_argument("query", help="Type of place to search for")
    nearby_parser.add_argument(
        "-l", "--location", help="Center location (lat,lng or place name)"
    )
    nearby_parser.add_argument(
        "-n", "--limit", type=int, default=5, help="Number of results to show"
    )

    # Route command
    route_parser = subparsers.add_parser(
        "route", help="Get directions between two places"
    )
    route_parser.add_argument("origin", help="Starting location")
    route_parser.add_argument("destination", help="Destination")
    route_parser.add_argument(
        "-m",
        "--mode",
        choices=["driving", "walking", "bicycling", "transit"],
        default="driving",
        help="Travel mode",
    )
    route_parser.add_argument(
        "--departure-time",
        help="Departure time (e.g., '2025-06-23 04:00' or '2025-06-23T04:00:00Z')",
    )
    route_parser.add_argument(
        "--arrival-time",
        help="Desired arrival time (e.g., '2025-06-23 05:40' or '2025-06-23T05:40:00Z')",
    )

    args = parser.parse_args()

    if args.command == "setup":
        setup(args.api_key_command)
    elif args.command == "search":
        search(args.query)
    elif args.command == "nearby":
        nearby(args.query, args.location, args.limit)
    elif args.command == "route":
        route(
            args.origin,
            args.destination,
            args.mode,
            args.departure_time,
            args.arrival_time,
        )
    else:
        parser.print_help()


if __name__ == "__main__":
    main()
