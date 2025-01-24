#!/usr/bin/env python3

import argparse
import configparser
import subprocess
import sys
from pathlib import Path


def get_existing_wifi_entries() -> set[str]:
    """Get existing WiFi entries from Bitwarden."""
    try:
        process = subprocess.run(
            ["rbw", "list", "--fields", "folder,name"],
            capture_output=True,
            text=True,
            check=True,
        )

        existing_entries: set[str] = set()

        for line in process.stdout.splitlines():
            # Split by tab, handle both entries with and without folder
            parts = line.split("\t")
            if len(parts) == 2 and parts[0].strip() == "wifi":
                existing_entries.add(parts[1].strip())

    except subprocess.SubprocessError as e:
        print(f"Failed to list Bitwarden entries: {e}", file=sys.stderr)
        return set()
    else:
        return existing_entries


def read_connection_file(file_path: Path) -> str | None:
    """Read a connection file using sudo cat."""
    try:
        process = subprocess.run(
            ["sudo", "cat", str(file_path)], capture_output=True, text=True, check=True
        )
    except subprocess.SubprocessError as e:
        print(f"Failed to read {file_path}: {e}", file=sys.stderr)
        return None
    else:
        return process.stdout


def get_wifi_connections() -> list[dict[str, str]]:
    """Get all WiFi connections and their passwords from NetworkManager."""
    connections: list[dict[str, str]] = []
    nm_path = Path("/etc/NetworkManager/system-connections")

    if not nm_path.exists():
        print("Error: NetworkManager connections directory not found", file=sys.stderr)
        return connections

    for conn_file in nm_path.glob("*.nmconnection"):
        if not conn_file.is_file():
            continue

        file_content = read_connection_file(conn_file)
        if not file_content:
            continue

        config = configparser.ConfigParser()
        try:
            config.read_string(file_content)

            # Check if this is a WiFi connection
            if config.has_section("wifi"):
                ssid = config.get("wifi", "ssid", fallback=None)
                psk = config.get("wifi-security", "psk", fallback=None)

                if ssid and psk:
                    connections.append({"ssid": ssid, "password": psk})
        except configparser.Error as e:
            print(f"Error parsing {conn_file}: {e}", file=sys.stderr)
            continue

    return connections


def add_to_bitwarden(ssid: str, password: str, dry_run: bool = False) -> bool:
    """Add WiFi credentials to Bitwarden using rbw."""
    if dry_run:
        print("[DRY RUN] Would add to Bitwarden:")
        print(f"  Title: {ssid}")
        print("  Folder: wifi")
        print(f"  Password: {password}")
        return True

    try:
        # Use rbw to add the entry
        process = subprocess.run(
            ["rbw", "add", "--folder", "wifi", ssid],
            input=password,
            text=True,
            check=False,
        )

        if process.returncode != 0:
            print(f"Error adding {ssid} to Bitwarden", file=sys.stderr)
            return False

    except subprocess.SubprocessError as e:
        print(f"Failed to execute rbw for {ssid}: {e}", file=sys.stderr)
        return False
    else:
        return True


def parse_args() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Export NetworkManager WiFi passwords to Bitwarden"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be done without actually adding to Bitwarden",
    )
    return parser.parse_args()


def main() -> None:
    """Main function to export WiFi passwords to Bitwarden."""
    args = parse_args()

    if args.dry_run:
        print("Running in dry-run mode - no changes will be made to Bitwarden")
        print("----------------------------------------")

    # Get existing entries first
    existing_entries = get_existing_wifi_entries()

    # Get all WiFi connections
    connections = get_wifi_connections()

    if not connections:
        print("No WiFi connections found")
        sys.exit(0)

    # Add each connection to Bitwarden
    success_count = 0
    skipped_count = 0

    for conn in connections:
        if conn["ssid"] in existing_entries:
            print(f"Skipping {conn['ssid']} - already exists in Bitwarden")
            skipped_count += 1
            continue

        if add_to_bitwarden(conn["ssid"], conn["password"], args.dry_run):
            success_count += 1
            if not args.dry_run:
                print(f"Successfully added {conn['ssid']} to Bitwarden")

    if args.dry_run:
        print("\n----------------------------------------")
        print(f"[DRY RUN] Would export {success_count} new WiFi passwords to Bitwarden")
        print(f"[DRY RUN] Would skip {skipped_count} existing entries")
    else:
        print(f"\nExported {success_count} new WiFi passwords to Bitwarden")
        print(f"Skipped {skipped_count} existing entries")


if __name__ == "__main__":
    main()
