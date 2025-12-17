"""Shared utilities for package update scripts."""

import json
import subprocess
from pathlib import Path

__all__ = ["get_nix_hash", "read_srcs", "update_srcs", "write_srcs"]


def get_nix_hash(url: str) -> str:
    """Get SRI hash for a URL using nix-prefetch-url."""
    result = subprocess.run(
        ["nix-prefetch-url", url],
        capture_output=True,
        text=True,
        check=True,
    )
    hash_value = result.stdout.strip()

    sri_result = subprocess.run(
        ["nix", "hash", "to-sri", "--type", "sha256", hash_value],
        capture_output=True,
        text=True,
        check=True,
    )
    return sri_result.stdout.strip()


def read_srcs(pkg_dir: Path) -> dict:
    """Read current srcs.json, returning empty dict if not found."""
    srcs_file = pkg_dir / "srcs.json"
    if srcs_file.exists():
        return json.loads(srcs_file.read_text())
    return {}


def write_srcs(pkg_dir: Path, data: dict) -> None:
    """Write srcs.json with consistent formatting."""
    srcs_file = pkg_dir / "srcs.json"
    srcs_file.write_text(json.dumps(data, indent=2) + "\n")


def update_srcs(
    pkg_dir: Path, version: str, url: str, hash_value: str | None = None
) -> bool:
    """
    Update srcs.json if version changed.

    If hash_value is None, it will be fetched using nix-prefetch-url.
    Returns True if updated, False if already up to date.
    """
    current = read_srcs(pkg_dir)

    if current.get("version") == version:
        print("Already up to date")
        return False

    if hash_value is None:
        print("Fetching hash...")
        hash_value = get_nix_hash(url)

    write_srcs(pkg_dir, {"version": version, "url": url, "hash": hash_value})
    print(f"Updated to version {version}")
    return True
