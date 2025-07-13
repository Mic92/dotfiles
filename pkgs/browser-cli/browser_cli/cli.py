"""Command-line interface for browser-cli."""

import argparse
import asyncio
import json
import logging
import shutil
import sys
from pathlib import Path

from browser_cli.client import BrowserCLI
from browser_cli.commands import (
    BackCommand,
    ClickCommand,
    Command,
    CommonOptions,
    ConsoleCommand,
    DragCommand,
    ForwardCommand,
    HoverCommand,
    InstallHostCommand,
    KeyCommand,
    NavigateCommand,
    ScreenshotCommand,
    SelectCommand,
    SnapshotCommand,
    TypeCommand,
)
from browser_cli.errors import BrowserCLIError, InvalidCommandError


def install_native_host() -> None:
    """Install native messaging host for Firefox."""
    # Get the path to browser-cli-server executable
    server_path = shutil.which("browser-cli-server")
    if not server_path:
        print("Error: browser-cli-server not found in PATH", file=sys.stderr)
        sys.exit(1)

    # Define paths
    home = Path.home()
    host_dir = home / ".mozilla" / "native-messaging-hosts"
    host_file = host_dir / "io.thalheim.browser_cli.bridge.json"

    # Create directory if it doesn't exist
    host_dir.mkdir(parents=True, exist_ok=True)

    # Create the native messaging host manifest
    manifest = {
        "name": "io.thalheim.browser_cli.bridge",
        "description": "Browser CLI Bridge Server",
        "path": server_path,
        "type": "stdio",
        "allowed_extensions": ["browser-cli-controller@thalheim.io"],
    }

    # Write the manifest file
    with host_file.open("w") as f:
        json.dump(manifest, f, indent=2)

    print(f"Native messaging host installed successfully at {host_file}")
    print(f"Server path: {server_path}")


def create_parser() -> argparse.ArgumentParser:
    """Create and configure the argument parser."""
    parser = argparse.ArgumentParser(
        description="Control Firefox browser from the command line",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  browser-cli navigate https://example.com
  browser-cli click "Sign In"
  browser-cli type "input[name='username']" "myuser"
  browser-cli screenshot output.png
  browser-cli console
  browser-cli snapshot
        """,
    )

    subparsers = parser.add_subparsers(dest="command", help="Available commands")

    # Create all subparsers
    navigate_parser = subparsers.add_parser("navigate", help="Navigate to URL")
    navigate_parser.add_argument("url", help="URL to navigate to")

    back_parser = subparsers.add_parser("back", help="Go back in browser history")
    forward_parser = subparsers.add_parser("forward", help="Go forward in browser history")

    click_parser = subparsers.add_parser("click", help="Click an element")
    click_parser.add_argument("selector", help="CSS selector or text to find element")

    type_parser = subparsers.add_parser("type", help="Type text into an element")
    type_parser.add_argument("selector", help="CSS selector or text to find element")
    type_parser.add_argument("text", help="Text to type")

    hover_parser = subparsers.add_parser("hover", help="Hover over an element")
    hover_parser.add_argument("selector", help="CSS selector or text to find element")

    drag_parser = subparsers.add_parser("drag", help="Drag from one element to another")
    drag_parser.add_argument("start", help="Start element selector")
    drag_parser.add_argument("end", help="End element selector")

    select_parser = subparsers.add_parser("select", help="Select an option in a dropdown")
    select_parser.add_argument("selector", help="Select element selector")
    select_parser.add_argument("option", help="Option value to select")

    key_parser = subparsers.add_parser("key", help="Press a keyboard key")
    key_parser.add_argument("key", help="Key to press (e.g., Enter, Tab, Escape)")

    screenshot_parser = subparsers.add_parser("screenshot", help="Take a screenshot")
    screenshot_parser.add_argument(
        "output",
        nargs="?",
        help="Output file (default: screenshot.png)",
    )

    console_parser = subparsers.add_parser("console", help="Get console logs from the page")
    snapshot_parser = subparsers.add_parser("snapshot", help="Get ARIA snapshot of the page")
    subparsers.add_parser(
        "install-host",
        help="Install native messaging host for Firefox",
    )

    # List of all subparsers that need common arguments (all except install-host)
    subparsers_with_common_args = [
        navigate_parser,
        back_parser,
        forward_parser,
        click_parser,
        type_parser,
        hover_parser,
        drag_parser,
        select_parser,
        key_parser,
        screenshot_parser,
        console_parser,
        snapshot_parser,
    ]

    # Add common arguments to all relevant subparsers
    for subparser in subparsers_with_common_args:
        subparser.add_argument(
            "--socket",
            help="Unix socket path (default: $XDG_RUNTIME_DIR/browser-cli.sock)",
        )
        subparser.add_argument(
            "--debug",
            action="store_true",
            help="Enable debug logging",
        )

    return parser


def parse_args(argv: list[str] | None = None) -> Command:  # noqa: C901, PLR0911, PLR0912
    """Parse command line arguments and return appropriate command dataclass."""
    parser = create_parser()
    args = parser.parse_args(argv)

    if not args.command:
        parser.print_help()
        sys.exit(1)

    # Create common options for all commands
    common = CommonOptions(
        socket=getattr(args, "socket", None),
        debug=getattr(args, "debug", False),
    )

    # Map parsed arguments to command dataclasses
    match args.command:
        case "navigate":
            return NavigateCommand(url=args.url, common=common)
        case "back":
            return BackCommand(common=common)
        case "forward":
            return ForwardCommand(common=common)
        case "click":
            return ClickCommand(selector=args.selector, common=common)
        case "type":
            return TypeCommand(selector=args.selector, text=args.text, common=common)
        case "hover":
            return HoverCommand(selector=args.selector, common=common)
        case "drag":
            return DragCommand(start=args.start, end=args.end, common=common)
        case "select":
            return SelectCommand(selector=args.selector, option=args.option, common=common)
        case "key":
            return KeyCommand(key=args.key, common=common)
        case "screenshot":
            return ScreenshotCommand(output=args.output, common=common)
        case "console":
            return ConsoleCommand(common=common)
        case "snapshot":
            return SnapshotCommand(common=common)
        case "install-host":
            return InstallHostCommand(common=common)
        case _:
            msg = f"Unknown command: {args.command}"
            raise InvalidCommandError(msg)


async def execute_command(cmd: Command) -> None:  # noqa: C901, PLR0912
    """Execute the given command using the browser client."""
    # Handle InstallHostCommand separately as it doesn't need a server
    if isinstance(cmd, InstallHostCommand):
        install_native_host()
        return

    # All other commands have common options
    client = BrowserCLI(cmd.common.socket)

    # Enable debug logging if requested
    if cmd.common.debug:
        logging.basicConfig(level=logging.DEBUG)

    match cmd:
        case NavigateCommand(url=url):
            await client.navigate(url)
        case BackCommand():
            await client.back()
        case ForwardCommand():
            await client.forward()
        case ClickCommand(selector=selector):
            await client.click(selector)
        case TypeCommand(selector=selector, text=text):
            await client.type_text(selector, text)
        case HoverCommand(selector=selector):
            await client.hover(selector)
        case DragCommand(start=start, end=end):
            await client.drag(start, end)
        case SelectCommand(selector=selector, option=option):
            await client.select(selector, option)
        case KeyCommand(key=key):
            await client.key(key)
        case ScreenshotCommand(output=output):
            await client.screenshot(output)
        case ConsoleCommand():
            await client.console()
        case SnapshotCommand():
            await client.snapshot()
        case _:
            msg = f"Unknown command type: {type(cmd)}"
            raise InvalidCommandError(msg)


def main() -> None:
    """Run the browser CLI."""
    try:
        cmd = parse_args()
        asyncio.run(execute_command(cmd))
    except BrowserCLIError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    except KeyboardInterrupt:
        print("\nInterrupted by user", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
