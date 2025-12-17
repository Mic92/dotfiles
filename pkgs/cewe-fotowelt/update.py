#!/usr/bin/env python3
"""Update cewe-fotowelt to the latest version."""

import gzip
import re
import sys
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from updater import get_nix_hash, read_srcs, write_srcs

KEY_ACCOUNT = "24441"
CLIENT_ID = "148"
LOCALE = "de_DE"


def get_latest_version() -> str:
    """Fetch the latest version from the CEWE download API."""
    affiliate_id = "x_x_x_x_6822_x_06822-BsEZQQWOPUVGg"
    api_url = f"https://dls.photoprintit.com/api/getClient/{KEY_ACCOUNT}-{LOCALE}/hps/{affiliate_id}/linux"

    with urllib.request.urlopen(api_url) as response:  # noqa: S310
        compressed_data = response.read()
        data = gzip.decompress(compressed_data)

        for encoding in ["iso-8859-1", "windows-1252", "utf-8", "latin-1"]:
            try:
                content = data.decode(encoding)
                match = re.search(r"HPS_VER\s*=\s*'([^']+)'", content)
                if match:
                    return match.group(1)
            except UnicodeDecodeError:
                continue

    msg = "Could not find version in API response"
    raise ValueError(msg)


def fetch_index(version: str) -> str:
    """Fetch the index file from CEWE servers."""
    index_url = f"https://dls.photoprintit.com/download/Data/{KEY_ACCOUNT}-{LOCALE}/hps/{CLIENT_ID}-index-{version}.txt"

    with urllib.request.urlopen(index_url) as response:  # noqa: S310
        content = response.read()
        for encoding in ["iso-8859-1", "windows-1252", "utf-8", "latin-1"]:
            try:
                return content.decode(encoding)
            except UnicodeDecodeError:
                continue
    msg = "Could not decode index file"
    raise ValueError(msg)


def parse_packages(index_content: str, version: str) -> list[dict[str, str]]:
    """Parse packages for Linux x64 and all platforms."""
    packages = []

    for raw_line in index_content.strip().split("\n"):
        line = raw_line.strip()
        if not line:
            continue

        parts = line.split(";")
        if len(parts) >= 4:
            path, _required, _desc, platform = parts[0:4]
            platform = platform.strip()

            if platform in ["l64", "a"] and not path.endswith(".zip"):
                filename = Path(path).name
                match = re.match(rf"148-(.+)-{re.escape(version)}_.*\.7z", filename)
                if match:
                    name = match.group(1)
                    url = f"https://dls.photoprintit.com/{path}"
                    packages.append({"name": name, "url": url})

    return packages


def main() -> None:
    pkg_dir = Path(__file__).parent
    current = read_srcs(pkg_dir)

    print("Fetching latest CEWE Fotowelt version...")
    version = get_latest_version()
    print(f"Latest version: {version}")

    if current.get("version") == version:
        print("Already up to date")
        return

    print(f"Fetching package index for {version}...")
    index_content = fetch_index(version)
    packages = parse_packages(index_content, version)
    print(f"Found {len(packages)} packages")

    print("Fetching hashes...")
    sources = []
    for pkg in packages:
        print(f"  {pkg['name']}...")
        hash_value = get_nix_hash(pkg["url"])
        sources.append({"name": pkg["name"], "url": pkg["url"], "hash": hash_value})

    write_srcs(
        pkg_dir,
        {
            "version": version,
            "keyAccId": KEY_ACCOUNT,
            "clientId": CLIENT_ID,
            "sources": sources,
        },
    )
    print(f"Updated to version {version}")


if __name__ == "__main__":
    main()
