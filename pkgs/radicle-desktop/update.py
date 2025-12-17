#!/usr/bin/env python3
"""Update radicle-desktop by mirroring from files.radicle.xyz to GitHub releases."""

import json
import subprocess
import sys
import tempfile
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from updater import get_nix_hash, read_srcs, write_srcs

GITHUB_REPO = "Mic92/dotfiles"
RELEASE_TAG = "radicle-desktop-mirror"


def get_latest_version() -> dict:
    """Fetch latest version info from radicle."""
    url = "https://files.radicle.xyz/releases/radicle-desktop/latest/latest.json"
    with urllib.request.urlopen(url) as response:  # noqa: S310
        return json.loads(response.read().decode())


def mirror_to_github(version: str, sha: str) -> str:
    """Download from radicle and upload to GitHub releases."""
    dmg_url = "https://files.radicle.xyz/releases/radicle-desktop/latest/radicle-desktop-aarch64.dmg"
    filename = f"radicle-desktop-{version}-aarch64.dmg"

    with tempfile.TemporaryDirectory() as tmpdir:
        local_path = Path(tmpdir) / filename
        print(f"Downloading {dmg_url}...")

        urllib.request.urlretrieve(dmg_url, local_path)  # noqa: S310

        body = json.dumps({"version": version, "sha": sha})
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
                    "delete-asset",
                    RELEASE_TAG,
                    filename,
                    "--repo",
                    GITHUB_REPO,
                    "--yes",
                ],
                check=False,
                capture_output=True,
            )
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
                    "Radicle Desktop Mirror",
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

    print("Fetching latest radicle-desktop version...")
    latest = get_latest_version()
    version = latest["version"]
    sha = latest["sha"]
    print(f"Latest version: {version} ({sha[:8]})")

    if current.get("version") == version:
        print("Already up to date")
        return

    url = mirror_to_github(version, sha)
    print(f"Mirrored to: {url}")

    print("Fetching hash...")
    hash_value = get_nix_hash(url)

    write_srcs(pkg_dir, {"version": version, "url": url, "hash": hash_value})
    print(f"Updated to version {version}")


if __name__ == "__main__":
    main()
