#!/usr/bin/env python3
"""Update librewolf-macos to the latest release from GitLab."""

import json
import sys
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from updater import update_srcs


def get_latest_release() -> dict:
    """Fetch latest release info from GitLab API."""
    api_url = "https://gitlab.com/api/v4/projects/44042130/releases"

    with urllib.request.urlopen(api_url) as response:  # noqa: S310
        releases = json.loads(response.read().decode())

    for release in releases:
        tag = release["tag_name"]
        version = tag.lstrip("v")

        for link in release.get("assets", {}).get("links", []):
            if "macos-arm64-package.dmg" in link.get("url", ""):
                return {"version": version}

    msg = "No suitable release found"
    raise ValueError(msg)


def main() -> None:
    pkg_dir = Path(__file__).parent

    print("Fetching latest librewolf release...")
    release = get_latest_release()
    version = release["version"]
    print(f"Latest version: {version}")

    url = f"https://gitlab.com/api/v4/projects/44042130/packages/generic/librewolf/{version}/librewolf-{version}-macos-arm64-package.dmg"

    update_srcs(pkg_dir, version, url)


if __name__ == "__main__":
    main()
