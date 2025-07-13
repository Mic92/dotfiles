"""Command dataclasses for browser-cli."""

from dataclasses import dataclass, field
from enum import Enum


class SelectorType(Enum):
    """Types of selectors supported for finding elements."""

    CSS = "css"
    TEXT = "text"
    ARIA_LABEL = "aria-label"
    PLACEHOLDER = "placeholder"


@dataclass
class CommonOptions:
    """Common options shared by all commands that need socket connection."""

    socket: str | None = None
    debug: bool = False
    tab: str | None = None  # Tab ID for targeting specific tabs


@dataclass
class NavigateCommand:
    """Navigate to a URL."""

    url: str
    common: CommonOptions


@dataclass
class BackCommand:
    """Go back in browser history."""

    common: CommonOptions


@dataclass
class ForwardCommand:
    """Go forward in browser history."""

    common: CommonOptions


@dataclass
class ClickCommand:
    """Click an element."""

    selector: str
    common: CommonOptions
    selector_type: SelectorType = SelectorType.CSS


@dataclass
class TypeCommand:
    """Type text into an element."""

    selector: str
    text: str
    common: CommonOptions
    selector_type: SelectorType = SelectorType.CSS


@dataclass
class HoverCommand:
    """Hover over an element."""

    selector: str
    common: CommonOptions
    selector_type: SelectorType = SelectorType.CSS


@dataclass
class DragCommand:
    """Drag from one element to another."""

    start: str
    end: str
    common: CommonOptions
    selector_type: SelectorType = SelectorType.CSS


@dataclass
class SelectCommand:
    """Select an option in a dropdown."""

    selector: str
    option: str
    common: CommonOptions
    selector_type: SelectorType = SelectorType.CSS


@dataclass
class KeyCommand:
    """Press a keyboard key."""

    key: str
    common: CommonOptions


@dataclass
class ScreenshotCommand:
    """Take a screenshot."""

    common: CommonOptions
    output: str | None = None


@dataclass
class ConsoleCommand:
    """Get console logs from the page."""

    common: CommonOptions


@dataclass
class SnapshotCommand:
    """Get ARIA snapshot of the page."""

    common: CommonOptions


@dataclass
class InstallHostCommand:
    """Install native messaging host for Firefox."""

    common: CommonOptions


@dataclass
class ListTabsCommand:
    """List all tabs managed by the extension."""

    common: CommonOptions


@dataclass
class NewTabCommand:
    """Create a new tab managed by the extension."""

    url: str | None = None
    common: CommonOptions = field(default_factory=CommonOptions)


@dataclass
class EvalCommand:
    """Evaluate JavaScript expression and return result as JSON."""

    expression: str
    common: CommonOptions


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
    | KeyCommand
    | ScreenshotCommand
    | ConsoleCommand
    | SnapshotCommand
    | InstallHostCommand
    | ListTabsCommand
    | NewTabCommand
    | EvalCommand
)
