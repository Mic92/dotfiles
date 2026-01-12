"""Command-line interface for browser-cli.

Provides a minimal interface similar to pexpect-cli:
- Execute JavaScript directly via stdin
- List managed tabs
- install-host for setup
"""

import argparse
import asyncio
import json
import logging
import shutil
import sys
from pathlib import Path
from typing import Any

from browser_cli.client import BrowserClient
from browser_cli.errors import BrowserCLIError


def install_native_host() -> None:
    """Install native messaging host for Firefox."""
    server_path = shutil.which("browser-cli-server")
    if not server_path:
        print("Error: browser-cli-server not found in PATH", file=sys.stderr)
        sys.exit(1)

    home = Path.home()

    # Create a wrapper script that doesn't hardcode nix store paths
    wrapper_dir = home / ".local" / "bin"
    wrapper_dir.mkdir(parents=True, exist_ok=True)
    wrapper_path = wrapper_dir / "browser-cli-server-wrapper"

    wrapper_content = f"""#!/usr/bin/env bash
exec "{server_path}" "$@"
"""
    wrapper_path.write_text(wrapper_content)
    wrapper_path.chmod(0o755)

    host_dirs = []
    if sys.platform == "darwin":
        app_support = home / "Library" / "Application Support"
        host_dirs.append(app_support / "Mozilla" / "NativeMessagingHosts")
        host_dirs.append(app_support / "LibreWolf" / "NativeMessagingHosts")
    else:
        host_dirs.append(home / ".mozilla" / "native-messaging-hosts")

    for host_dir in host_dirs:
        host_dir.mkdir(parents=True, exist_ok=True)
        host_file = host_dir / "io.thalheim.browser_cli.bridge.json"

        manifest = {
            "name": "io.thalheim.browser_cli.bridge",
            "description": "Browser CLI Bridge Server",
            "path": str(wrapper_path),
            "type": "stdio",
            "allowed_extensions": ["browser-cli-controller@thalheim.io"],
        }

        with host_file.open("w") as f:
            json.dump(manifest, f, indent=2)

        print(f"Native messaging host installed successfully at {host_file}")
    print(f"Using wrapper script at: {wrapper_path}")


def _format_element(el: dict[str, Any]) -> str:
    """Format a single element for display."""
    line = f"[{el['ref']}] {el['role']}"
    if el.get("name"):
        line += f' "{el["name"]}"'
    if el.get("attrs"):
        line += f" [{', '.join(el['attrs'])}]"
    if el.get("value") is not None:
        line += f' value="{el["value"]}"'
    return line


def _format_diff_section(
    lines: list[str],
    label: str,
    items: list[dict[str, Any]],
    prefix: str,
) -> None:
    """Format a diff section (added/removed)."""
    if not items:
        return
    lines.extend(["", f"{label} ({len(items)}):"])
    lines.extend(f"  {prefix} {_format_element(el)}" for el in items)


def _format_diff(result: dict[str, Any]) -> str | None:
    """Format a SnapshotDiff for display. Returns None if not a diff."""
    if "added" not in result or "removed" not in result or "changed" not in result:
        return None

    added, removed, changed = result["added"], result["removed"], result["changed"]
    url_changed, title_changed = result.get("urlChanged"), result.get("titleChanged")

    if not (url_changed or title_changed or added or removed or changed):
        return "No changes"

    lines: list[str] = []

    if url_changed:
        lines.append(f"URL: {result['oldUrl']} → {result['newUrl']}")
    if title_changed:
        lines.append(f'Title: "{result["oldTitle"]}" → "{result["newTitle"]}"')

    _format_diff_section(lines, "Added", added, "+")
    _format_diff_section(lines, "Removed", removed, "-")

    if changed:
        lines.extend(["", f"Changed ({len(changed)}):"])
        for item in changed:
            lines.append(f"  ~ {_format_element(item['element'])}")
            lines.extend(f"      {c}" for c in item["changes"])

    return "\n".join(lines)


def _format_snapshot_dict(result: dict[str, Any]) -> str | None:
    """Format a snapshot dict for display. Returns None if not a snapshot."""
    # Check if it's a SnapshotDiff object
    diff_result = _format_diff(result)
    if diff_result is not None:
        return diff_result

    # Check if it's a Snapshot object
    if "url" in result and "title" in result and "elements" in result:
        lines = [f"Page: {result['title']}", f"URL: {result['url']}", ""]
        lines.extend(_format_element(el) for el in result["elements"])
        return "\n".join(lines)

    # Check if it's a single element
    if "ref" in result and "role" in result:
        return _format_element(result)

    return None


def _format_element_list(result: list[Any]) -> str | None:
    """Format a list of elements for display. Returns None if not element list."""
    if not result or not isinstance(result[0], dict):
        return None

    if "ref" not in result[0] or "role" not in result[0]:
        return None

    return "\n".join(_format_element(el) for el in result)


def format_snapshot(
    result: dict[str, object] | list[object] | str | float | None,
) -> str:
    """Format a snapshot result for display."""
    if result is None:
        return ""

    if isinstance(result, str):
        return result

    if isinstance(result, dict):
        formatted = _format_snapshot_dict(result)
        if formatted is not None:
            return formatted

    # For arrays of elements
    if isinstance(result, list):
        formatted = _format_element_list(result)
        if formatted is not None:
            return formatted

    # Default: JSON format
    return json.dumps(result, indent=2)


async def exec_js(tab_id: str | None, code: str, socket: str | None) -> None:
    """Execute JavaScript code in a browser tab."""
    client = BrowserClient(socket)
    result = await client.exec_js(code, tab_id)
    if result is not None:
        print(format_snapshot(result))


async def list_tabs(socket: str | None) -> None:
    """List all managed tabs."""
    client = BrowserClient(socket)
    tabs = await client.list_tabs()
    if not tabs:
        print("No managed tabs")
        return
    for tab in tabs:
        tab_id = tab.get("id", "unknown")
        url = tab.get("url", "about:blank")
        title = tab.get("title", "Untitled")
        active = " (active)" if tab.get("active") else ""
        print(f"{tab_id}: {title}{active}")
        print(f"       {url}")


def create_parser() -> argparse.ArgumentParser:
    """Create and configure the argument parser."""
    parser = argparse.ArgumentParser(
        description="Control Firefox browser from the command line",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # List managed tabs
  browser-cli --list

  # Open page and get snapshot
  browser-cli <<'EOF'
  await tab("https://example.com")
  snap()
  EOF

  # Form filling with refs
  browser-cli <<'EOF'
  await type(1, "user@test.com")
  await type(2, "secret123")
  await click(3)
  EOF

  # Wait for dynamic content
  browser-cli <<'EOF'
  await click(5)
  await wait("text", "Success")
  snap()
  EOF

Available JS API:
  Interaction (use refs from snap()):
    click(ref)           - Click element
    click(ref, {double}) - Double click
    type(ref, text)      - Type into input
    type(ref, text, {clear}) - Clear first
    hover(ref)           - Hover element
    drag(from, to)       - Drag and drop
    select(ref, value)   - Select option
    key(name)            - Press key

  Inspection:
    snap()               - Get page snapshot (diff after first call)
    snap({full: true})   - Force full snapshot
    snap({forms: true})  - Filter: form elements only
    snap({links: true})  - Filter: links only
    snap({text: "..."})  - Filter: by text
    logs()               - Get console logs

  Waiting:
    wait(ms)             - Wait milliseconds
    wait("idle")         - Wait for DOM to stabilize
    wait("text", str)    - Wait for text to appear
    wait("gone", str)    - Wait for text to disappear

  Media:
    shot()               - Screenshot (returns data URL)
    shot(path)           - Screenshot to file
    download(url)        - Download file to ~/Downloads
    download(url, name)  - Download with custom filename

  Tabs:
    tab()                - New tab
    tab(url)             - New tab with URL
    tabs()               - List tabs
        """,
    )

    parser.add_argument(
        "tab_id",
        nargs="?",
        help="Tab ID to execute code in. Code is read from stdin.",
    )
    parser.add_argument(
        "--list",
        action="store_true",
        help="List all managed tabs",
    )
    parser.add_argument(
        "--install-host",
        action="store_true",
        help="Install native messaging host for Firefox",
    )
    parser.add_argument(
        "--socket",
        help="Unix socket path (default: $XDG_RUNTIME_DIR/browser-cli.sock)",
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Enable debug logging",
    )

    return parser


def main() -> None:
    """Run the browser CLI."""
    parser = create_parser()
    args = parser.parse_args()

    if args.debug:
        logging.basicConfig(level=logging.DEBUG)

    try:
        if args.install_host:
            install_native_host()
        elif args.list:
            asyncio.run(list_tabs(args.socket))
        elif args.tab_id or not sys.stdin.isatty():
            code = sys.stdin.read()
            if not code.strip():
                print("Error: No JavaScript code provided on stdin", file=sys.stderr)
                sys.exit(1)
            asyncio.run(exec_js(args.tab_id, code, args.socket))
        else:
            parser.print_help()
            sys.exit(1)

    except BrowserCLIError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    except KeyboardInterrupt:
        print("\nInterrupted by user", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
