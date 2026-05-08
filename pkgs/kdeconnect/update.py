#!/usr/bin/env python3
"""Update kdeconnect by mirroring KDE CI builds to GitHub releases.

KDE's CI CDN keeps only the single most recent build. Pinning that URL in
srcs.json breaks as soon as a newer build replaces it. To keep fetchurl
reproducible we mirror the DMG to a GitHub release on this repo.
"""

import json
import re
import subprocess
import sys
import tempfile
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from updater import get_nix_hash, read_srcs, write_srcs

GITHUB_REPO = "Mic92/dotfiles"
RELEASE_TAG = "kdeconnect-mirror"
BASE_URL = "https://cdn.kde.org/ci-builds/network/kdeconnect-kde/master/macos-arm64/"


def get_latest_build() -> dict:
    """Fetch latest build number from KDE CI directory listing."""
    with urllib.request.urlopen(BASE_URL) as response:  # noqa: S310
        html = response.read().decode()

    pattern = r"kdeconnect-kde-master-(\d+)-macos-clang-arm64\.dmg"
    builds = [int(m) for m in re.findall(pattern, html)]

    if not builds:
        msg = "No builds found"
        raise ValueError(msg)

    latest = max(builds)
    filename = f"kdeconnect-kde-master-{latest}-macos-clang-arm64.dmg"
    return {"version": str(latest), "url": BASE_URL + filename, "filename": filename}


def mirror_to_github(version: str, src_url: str, filename: str) -> str:
    """Download from KDE CDN and upload to GitHub releases."""
    with tempfile.TemporaryDirectory() as tmpdir:
        local_path = Path(tmpdir) / filename
        print(f"Downloading {src_url}...")
        urllib.request.urlretrieve(src_url, local_path)  # noqa: S310

        body = json.dumps({"version": version, "source": src_url})
        release_exists = (
            subprocess.run(
                ["gh", "release", "view", RELEASE_TAG, "--repo", GITHUB_REPO],
                check=False,
                capture_output=True,
            ).returncode
            == 0
        )

        if release_exists:
            subprocess.run(
                [
                    "gh",
                    "release",
                    "edit",
                    RELEASE_TAG,
                    "--repo",
                    GITHUB_REPO,
                    "--notes",
                    body,
                ],
                check=True,
            )
        else:
            subprocess.run(
                [
                    "gh",
                    "release",
                    "create",
                    RELEASE_TAG,
                    "--repo",
                    GITHUB_REPO,
                    "--title",
                    "KDE Connect Mirror",
                    "--notes",
                    body,
                ],
                check=True,
            )

        print("Uploading to GitHub release...")
        subprocess.run(
            [
                "gh",
                "release",
                "upload",
                RELEASE_TAG,
                str(local_path),
                "--repo",
                GITHUB_REPO,
                "--clobber",
            ],
            check=True,
        )

    return (
        f"https://github.com/{GITHUB_REPO}/releases/download/{RELEASE_TAG}/{filename}"
    )


def main() -> None:
    pkg_dir = Path(__file__).parent
    current = read_srcs(pkg_dir)

    print("Fetching latest kdeconnect build...")
    build = get_latest_build()
    version = build["version"]
    print(f"Latest build: {version}")

    if current.get("version") == version:
        print("Already up to date")
        return

    url = mirror_to_github(version, build["url"], build["filename"])
    print(f"Mirrored to: {url}")

    print("Fetching hash...")
    hash_value = get_nix_hash(url)

    write_srcs(pkg_dir, {"version": version, "url": url, "hash": hash_value})
    print(f"Updated to version {version}")


if __name__ == "__main__":
    main()
