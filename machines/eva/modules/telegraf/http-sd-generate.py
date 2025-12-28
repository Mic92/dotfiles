# ruff: noqa: INP001
"""Convert HTTP SD JSON to Telegraf TOML config."""

import json
import sys
import tempfile
import urllib.request
from pathlib import Path
from typing import Any
from urllib.error import URLError


def atomic_write(path: Path, content: str) -> None:
    """Write content to file atomically via temp file + rename."""
    with tempfile.NamedTemporaryFile(
        mode="w", dir=path.parent, delete=False, suffix=".tmp"
    ) as tmp:
        tmp.write(content)
        tmp_path = Path(tmp.name)
    tmp_path.rename(path)


def fetch_and_cache(url: str, cache_file: Path) -> list[dict[str, Any]]:
    """Fetch JSON from URL and cache it. Falls back to cache on failure."""
    if not url.startswith(("https://", "http://")):
        print(f"Invalid URL scheme: {url}", file=sys.stderr)
        return load_cache(cache_file)

    try:
        with urllib.request.urlopen(url, timeout=30) as response:  # noqa: S310
            data: list[dict[str, Any]] = json.load(response)
    except (URLError, TimeoutError) as e:
        print(f"Failed to fetch {url}: {e}", file=sys.stderr)
        return load_cache(cache_file)
    except json.JSONDecodeError as e:
        print(f"Failed to parse JSON from {url}: {e}", file=sys.stderr)
        return load_cache(cache_file)
    else:
        # Cache the successful response
        atomic_write(cache_file, json.dumps(data))
        return data


def load_cache(cache_file: Path) -> list[dict[str, Any]]:
    """Load cached JSON or return empty list."""
    if not cache_file.exists():
        return []

    try:
        data: list[dict[str, Any]] = json.loads(cache_file.read_text())
    except json.JSONDecodeError:
        print(f"Cache file {cache_file} is corrupted", file=sys.stderr)
        return []
    else:
        print(f"Using cached data from {cache_file}", file=sys.stderr)
        return data


def main() -> None:
    if len(sys.argv) != 4:
        print(f"Usage: {sys.argv[0]} <name> <url> <output_dir>", file=sys.stderr)
        sys.exit(1)

    name: str = sys.argv[1]
    url: str = sys.argv[2]
    output_dir: Path = Path(sys.argv[3])
    output_file: Path = output_dir / f"{name}.conf"
    cache_file: Path = output_dir / f"{name}.json"

    data: list[dict[str, Any]] = fetch_and_cache(url, cache_file)

    # Extract hosts from targets (format: "host.r:9273" -> "host.r")
    hosts: list[str] = []
    org: str = name
    for entry in data:
        if "labels" in entry and "org" in entry["labels"]:
            org = entry["labels"]["org"]
        for target in entry.get("targets", []):
            # Strip port suffix
            host: str = target.rsplit(":", 1)[0]
            hosts.append(host)

    lines: list[str] = [
        f"# Auto-generated from {url}",
        "",
    ]

    if not hosts:
        print(f"No hosts found for {name}", file=sys.stderr)
        lines.append("# No targets found")
        atomic_write(output_file, "\n".join(lines))
        sys.exit(0)

    # Generate ping inputs
    for host in hosts:
        host_label = host.removesuffix(".r")
        lines.extend(
            [
                "[[inputs.ping]]",
                '  method = "native"',
                f'  urls = ["6.{host}"]',
                "  ipv6 = true",
                "  [inputs.ping.tags]",
                f'    org = "{org}"',
                f'    host = "{host_label}"',
                "",
            ]
        )

    # Generate net_response (SSH) inputs
    for host in hosts:
        lines.extend(
            [
                "[[inputs.net_response]]",
                '  protocol = "tcp"',
                f'  address = "{host}:22"',
                '  send = "SSH-2.0-Telegraf"',
                '  expect = "SSH-2.0"',
                '  timeout = "10s"',
                "  [inputs.net_response.tags]",
                f'    org = "{org}"',
                f'    host = "{host}"',
                "",
            ]
        )

    atomic_write(output_file, "\n".join(lines))
    print(f"Generated config for {len(hosts)} hosts in {name}")


if __name__ == "__main__":
    main()
