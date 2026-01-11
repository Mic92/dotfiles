"""Browser CLI - Control Firefox from the command line.

Provides a minimal CLI that executes JavaScript via stdin, with a rich
JS API for browser automation.
"""

from browser_cli.bridge import NativeMessagingBridge
from browser_cli.cli import main
from browser_cli.client import BrowserClient

__all__ = ["BrowserClient", "NativeMessagingBridge", "main"]
