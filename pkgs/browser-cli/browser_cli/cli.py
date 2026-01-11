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


async def exec_js(tab_id: str | None, code: str, socket: str | None) -> None:
    """Execute JavaScript code in a browser tab."""
    client = BrowserClient(socket)
    result = await client.exec_js(code, tab_id)
    if result is not None:
        if isinstance(result, str):
            print(result)
        else:
            print(json.dumps(result, indent=2))


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
  # Execute JavaScript in the active tab (reads from stdin)
  echo 'document.title' | browser-cli

  # Execute in a specific tab
  echo 'navigate("https://example.com")' | browser-cli abc123

  # List all managed tabs
  browser-cli --list

  # Complex automation with heredoc
  browser-cli <<'EOF'
  await navigate("https://example.com");
  await click("input[name='q']");
  await type("input[name='q']", "hello world");
  await key("Enter");
  EOF

  # Get page snapshot
  echo 'snapshot()' | browser-cli

  # Take a screenshot
  echo 'screenshot("output.png")' | browser-cli

  # Tab management via JS API
  echo 'newTab("https://example.com")' | browser-cli
  echo 'closeTab()' | browser-cli abc123

  # Install native messaging host (one-time setup)
  browser-cli --install-host

Available JS API:
  navigate(url)              - Navigate to URL
  back()                     - Go back in history
  forward()                  - Go forward in history
  click(selector, type?)     - Click element (type: css|text|aria-label|placeholder)
  type(selector, text, type?) - Type into element
  hover(selector, type?)     - Hover over element
  drag(start, end, type?)    - Drag from start to end element
  select(selector, option, type?) - Select dropdown option
  key(key)                   - Press keyboard key
  screenshot(path?)          - Take screenshot
  snapshot(offset?, limit?)  - Get ARIA accessibility tree
  console()                  - Get console logs
  newTab(url?)               - Create new tab, returns tab ID
  closeTab(tabId?)           - Close tab (current if no ID)
  listTabs()                 - List managed tabs
  eval(expr)                 - Evaluate JS expression
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
