"""Browser CLI - Control Firefox through WebSocket connection."""

from browser_cli.bridge import NativeMessagingBridge
from browser_cli.cli import main

__all__ = ["NativeMessagingBridge", "main"]
