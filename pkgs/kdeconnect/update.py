#!/usr/bin/env python3
"""Update kdeconnect to the latest CI build from KDE."""

import re
import sys
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from updater import update_srcs


def get_latest_build() -> dict:
    """Fetch latest build number from KDE CI directory listing."""
    base_url = (
        "https://cdn.kde.org/ci-builds/network/kdeconnect-kde/master/macos-arm64/"
    )

    with urllib.request.urlopen(base_url) as response:  # noqa: S310
        html = response.read().decode()

    pattern = r"kdeconnect-kde-master-(\d+)-macos-clang-arm64\.dmg"
    builds = [int(m) for m in re.findall(pattern, html)]

    if not builds:
        msg = "No builds found"
        raise ValueError(msg)

    latest = max(builds)
    url = f"{base_url}kdeconnect-kde-master-{latest}-macos-clang-arm64.dmg"

    return {"version": str(latest), "url": url}


def main() -> None:
    pkg_dir = Path(__file__).parent

    print("Fetching latest kdeconnect build...")
    build = get_latest_build()
    version = build["version"]
    url = build["url"]
    print(f"Latest build: {version}")

    update_srcs(pkg_dir, version, url)


if __name__ == "__main__":
    main()
