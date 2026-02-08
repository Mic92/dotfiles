#!/usr/bin/env python3
"""Update blueutil to the latest GitHub release."""

import json
import sys
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from updater import get_nix_hash_unpack, read_srcs, write_srcs


def get_latest_version() -> str:
    """Fetch latest release tag from GitHub API."""
    url = "https://api.github.com/repos/toy/blueutil/releases/latest"

    with urllib.request.urlopen(url) as response:  # noqa: S310
        data = json.loads(response.read().decode())

    tag: str = data["tag_name"]
    return tag.lstrip("v")


def main() -> None:
    pkg_dir = Path(__file__).parent

    print("Fetching latest blueutil version...")
    version = get_latest_version()
    print(f"Latest version: {version}")

    current = read_srcs(pkg_dir)
    if current.get("version") == version:
        print("Already up to date")
        return

    archive_url = f"https://github.com/toy/blueutil/archive/refs/tags/v{version}.tar.gz"
    print("Fetching hash...")
    hash_value = get_nix_hash_unpack(archive_url)

    write_srcs(pkg_dir, {"version": version, "hash": hash_value})
    print(f"Updated to version {version}")


if __name__ == "__main__":
    main()
