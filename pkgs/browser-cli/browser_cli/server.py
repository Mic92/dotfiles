#!/usr/bin/env python3
"""Native messaging host that bridges browser extension and CLI.

The browser extension communicates via native messaging (stdin/stdout).
The CLI connects via Unix socket using JSON lines protocol.
"""

import asyncio
import contextlib
import logging
import logging.handlers
import os
import signal
import sys
import tempfile
from pathlib import Path

from browser_cli.bridge import NativeMessagingBridge

logger = logging.getLogger(__name__)


def setup_logging() -> None:
    """Set up logging to both terminal and file."""
    # Get XDG data home
    xdg_data_home = os.environ.get("XDG_DATA_HOME", str(Path.home() / ".local" / "share"))
    log_dir = Path(xdg_data_home) / "browser-cli"
    log_dir.mkdir(parents=True, exist_ok=True)
    log_file = log_dir / "server.log"

    # Set up root logger
    root_logger = logging.getLogger()
    root_logger.setLevel(logging.INFO)

    # File handler only - stdout is used for native messaging
    file_handler = logging.handlers.RotatingFileHandler(
        log_file,
        maxBytes=10 * 1024 * 1024,  # 10MB
        backupCount=5,
        encoding="utf-8",
    )
    file_handler.setLevel(logging.INFO)
    file_formatter = logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
    file_handler.setFormatter(file_formatter)

    # Add handler to root logger
    root_logger.addHandler(file_handler)

    logger.info("Logging initialized. Log file: %s", log_file)


async def async_main() -> None:
    """Run the native messaging bridge."""
    bridge = NativeMessagingBridge()

    # Handle shutdown gracefully
    stop: asyncio.Future[None] = asyncio.Future()

    def signal_handler() -> None:
        stop.set_result(None)

    loop = asyncio.get_event_loop()
    for sig in (signal.SIGTERM, signal.SIGINT):
        loop.add_signal_handler(sig, signal_handler)

    # Get runtime directory for Unix socket
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR")
    if not runtime_dir:
        runtime_dir = tempfile.gettempdir()

    socket_path = Path(runtime_dir) / "browser-cli.sock"

    # Run bridge until stopped
    bridge_task = asyncio.create_task(bridge.start(socket_path))

    await asyncio.wait([bridge_task, stop], return_when=asyncio.FIRST_COMPLETED)

    # Cancel bridge task if still running
    if not bridge_task.done():
        bridge_task.cancel()
        with contextlib.suppress(asyncio.CancelledError):
            await bridge_task


def main() -> None:
    """Run the native messaging bridge server."""
    setup_logging()
    try:
        asyncio.run(async_main())
    except KeyboardInterrupt:
        logger.info("Server stopped by user")
        sys.exit(0)


if __name__ == "__main__":
    main()
