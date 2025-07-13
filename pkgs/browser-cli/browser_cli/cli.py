"""Command-line interface for browser-cli."""

import argparse
import asyncio
import sys

from browser_cli.client import BrowserCLI
from browser_cli.commands import (
    BackCommand,
    ClickCommand,
    Command,
    ConsoleCommand,
    DragCommand,
    ForwardCommand,
    HoverCommand,
    KeyCommand,
    NavigateCommand,
    ScreenshotCommand,
    SelectCommand,
    SnapshotCommand,
    TypeCommand,
    WaitCommand,
)
from browser_cli.errors import BrowserCLIError, InvalidCommandError


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

    parser.add_argument(
        "--server",
        default="ws://localhost:9223",
        help="WebSocket server URL (default: ws://localhost:9223)",
    )

    subparsers = parser.add_subparsers(dest="command", help="Available commands")

    # Navigate command
    navigate_parser = subparsers.add_parser("navigate", help="Navigate to URL")
    navigate_parser.add_argument("url", help="URL to navigate to")

    # Navigation commands
    subparsers.add_parser("back", help="Go back in browser history")
    subparsers.add_parser("forward", help="Go forward in browser history")

    # Click command
    click_parser = subparsers.add_parser("click", help="Click an element")
    click_parser.add_argument("selector", help="CSS selector or text to find element")

    # Type command
    type_parser = subparsers.add_parser("type", help="Type text into an element")
    type_parser.add_argument("selector", help="CSS selector or text to find element")
    type_parser.add_argument("text", help="Text to type")

    # Hover command
    hover_parser = subparsers.add_parser("hover", help="Hover over an element")
    hover_parser.add_argument("selector", help="CSS selector or text to find element")

    # Drag command
    drag_parser = subparsers.add_parser("drag", help="Drag from one element to another")
    drag_parser.add_argument("start", help="Start element selector")
    drag_parser.add_argument("end", help="End element selector")

    # Select command
    select_parser = subparsers.add_parser("select", help="Select an option in a dropdown")
    select_parser.add_argument("selector", help="Select element selector")
    select_parser.add_argument("option", help="Option value to select")

    # Wait command
    wait_parser = subparsers.add_parser("wait", help="Wait for specified seconds")
    wait_parser.add_argument("seconds", type=float, help="Seconds to wait")

    # Key command
    key_parser = subparsers.add_parser("key", help="Press a keyboard key")
    key_parser.add_argument("key", help="Key to press (e.g., Enter, Tab, Escape)")

    # Screenshot command
    screenshot_parser = subparsers.add_parser("screenshot", help="Take a screenshot")
    screenshot_parser.add_argument(
        "output",
        nargs="?",
        help="Output file (default: screenshot.png)",
    )

    # Console command
    subparsers.add_parser("console", help="Get console logs from the page")

    # Snapshot command
    subparsers.add_parser("snapshot", help="Get ARIA snapshot of the page")

    return parser


def parse_args(argv: list[str] | None = None) -> Command:  # noqa: C901, PLR0911, PLR0912
    """Parse command line arguments and return appropriate command dataclass."""
    parser = create_parser()
    args = parser.parse_args(argv)

    if not args.command:
        parser.print_help()
        sys.exit(1)

    # Map parsed arguments to command dataclasses
    match args.command:
        case "navigate":
            return NavigateCommand(server=args.server, url=args.url)
        case "back":
            return BackCommand(server=args.server)
        case "forward":
            return ForwardCommand(server=args.server)
        case "click":
            return ClickCommand(server=args.server, selector=args.selector)
        case "type":
            return TypeCommand(server=args.server, selector=args.selector, text=args.text)
        case "hover":
            return HoverCommand(server=args.server, selector=args.selector)
        case "drag":
            return DragCommand(server=args.server, start=args.start, end=args.end)
        case "select":
            return SelectCommand(server=args.server, selector=args.selector, option=args.option)
        case "wait":
            return WaitCommand(server=args.server, seconds=args.seconds)
        case "key":
            return KeyCommand(server=args.server, key=args.key)
        case "screenshot":
            return ScreenshotCommand(server=args.server, output=args.output)
        case "console":
            return ConsoleCommand(server=args.server)
        case "snapshot":
            return SnapshotCommand(server=args.server)
        case _:
            msg = f"Unknown command: {args.command}"
            raise InvalidCommandError(msg)


async def execute_command(cmd: Command) -> None:  # noqa: C901, PLR0912
    """Execute the given command using the browser client."""
    client = BrowserCLI(cmd.server)

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
        case WaitCommand(seconds=seconds):
            await client.wait(seconds)
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
