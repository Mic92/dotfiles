"""Command dataclasses for browser-cli."""

from dataclasses import dataclass


@dataclass
class BaseCommand:
    """Base command with common server parameter."""

    server: str = "ws://localhost:9223"


@dataclass
class NavigateCommand(BaseCommand):
    """Navigate to a URL."""

    url: str


@dataclass
class BackCommand(BaseCommand):
    """Go back in browser history."""


@dataclass
class ForwardCommand(BaseCommand):
    """Go forward in browser history."""


@dataclass
class ClickCommand(BaseCommand):
    """Click an element."""

    selector: str


@dataclass
class TypeCommand(BaseCommand):
    """Type text into an element."""

    selector: str
    text: str


@dataclass
class HoverCommand(BaseCommand):
    """Hover over an element."""

    selector: str


@dataclass
class DragCommand(BaseCommand):
    """Drag from one element to another."""

    start: str
    end: str


@dataclass
class SelectCommand(BaseCommand):
    """Select an option in a dropdown."""

    selector: str
    option: str


@dataclass
class WaitCommand(BaseCommand):
    """Wait for specified seconds."""

    seconds: float


@dataclass
class KeyCommand(BaseCommand):
    """Press a keyboard key."""

    key: str


@dataclass
class ScreenshotCommand(BaseCommand):
    """Take a screenshot."""

    output: str | None = None


@dataclass
class ConsoleCommand(BaseCommand):
    """Get console logs from the page."""


@dataclass
class SnapshotCommand(BaseCommand):
    """Get ARIA snapshot of the page."""


# Type alias for all command types
Command = (
    NavigateCommand
    | BackCommand
    | ForwardCommand
    | ClickCommand
    | TypeCommand
    | HoverCommand
    | DragCommand
    | SelectCommand
    | WaitCommand
    | KeyCommand
    | ScreenshotCommand
    | ConsoleCommand
    | SnapshotCommand
)
