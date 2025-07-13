"""Command dataclasses for browser-cli."""

from dataclasses import dataclass


@dataclass
class NavigateCommand:
    """Navigate to a URL."""

    url: str
    server: str = "ws://localhost:9223"


@dataclass
class BackCommand:
    """Go back in browser history."""

    server: str = "ws://localhost:9223"


@dataclass
class ForwardCommand:
    """Go forward in browser history."""

    server: str = "ws://localhost:9223"


@dataclass
class ClickCommand:
    """Click an element."""

    selector: str
    server: str = "ws://localhost:9223"


@dataclass
class TypeCommand:
    """Type text into an element."""

    selector: str
    text: str
    server: str = "ws://localhost:9223"


@dataclass
class HoverCommand:
    """Hover over an element."""

    selector: str
    server: str = "ws://localhost:9223"


@dataclass
class DragCommand:
    """Drag from one element to another."""

    start: str
    end: str
    server: str = "ws://localhost:9223"


@dataclass
class SelectCommand:
    """Select an option in a dropdown."""

    selector: str
    option: str
    server: str = "ws://localhost:9223"


@dataclass
class WaitCommand:
    """Wait for specified seconds."""

    seconds: float
    server: str = "ws://localhost:9223"


@dataclass
class KeyCommand:
    """Press a keyboard key."""

    key: str
    server: str = "ws://localhost:9223"


@dataclass
class ScreenshotCommand:
    """Take a screenshot."""

    output: str | None = None
    server: str = "ws://localhost:9223"


@dataclass
class ConsoleCommand:
    """Get console logs from the page."""

    server: str = "ws://localhost:9223"


@dataclass
class SnapshotCommand:
    """Get ARIA snapshot of the page."""

    server: str = "ws://localhost:9223"


@dataclass
class InstallHostCommand:
    """Install native messaging host for Firefox."""

    # No server parameter needed for this command


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
    | InstallHostCommand
)
