#!/usr/bin/env python3
"""Native messaging host that bridges browser extension and CLI.

The browser extension communicates via native messaging (stdin/stdout).
The CLI connects via Unix socket using JSON lines protocol.
"""

import asyncio
import contextlib
import json
import logging
import logging.handlers
import os
import signal
import struct
import sys
import tempfile
from pathlib import Path
from typing import Any

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


class NativeMessagingBridge:
    """Bridge between browser extension (native messaging) and CLI (Unix socket)."""

    def __init__(self) -> None:
        """Initialize the bridge."""
        self.cli_clients: set[tuple[asyncio.StreamReader, asyncio.StreamWriter]] = set()
        self.pending_responses: dict[str, asyncio.Future] = {}
        self.message_counter = 0
        self.stdin_reader: asyncio.StreamReader | None = None
        self.stdout_writer: asyncio.StreamWriter | None = None

    async def setup_native_messaging(self) -> None:
        """Set up stdin/stdout for native messaging."""
        loop = asyncio.get_event_loop()

        # Create stream reader for stdin
        self.stdin_reader = asyncio.StreamReader()
        stdin_protocol = asyncio.StreamReaderProtocol(self.stdin_reader)
        await loop.connect_read_pipe(lambda: stdin_protocol, sys.stdin)

        # Create stream writer for stdout
        stdout_transport, stdout_protocol = await loop.connect_write_pipe(
            asyncio.streams.FlowControlMixin,
            sys.stdout,
        )
        self.stdout_writer = asyncio.StreamWriter(stdout_transport, stdout_protocol, None, loop)

    async def read_native_message(self) -> dict[str, Any] | None:
        """Read a message from native messaging stdin."""
        if not self.stdin_reader:
            return None

        try:
            # Read message length (4 bytes, little-endian)
            length_bytes = await self.stdin_reader.readexactly(4)
            length = struct.unpack("<I", length_bytes)[0]

            # Read message body
            message_bytes = await self.stdin_reader.readexactly(length)
            return json.loads(message_bytes.decode("utf-8"))
        except asyncio.IncompleteReadError:
            logger.info("Native messaging input closed")
            return None
        except Exception:
            logger.exception("Error reading native message")
            return None

    async def write_native_message(self, message: dict[str, Any]) -> None:
        """Write a message to native messaging stdout."""
        if not self.stdout_writer:
            return

        try:
            # Encode message
            message_bytes = json.dumps(message).encode("utf-8")

            # Write message length (4 bytes, little-endian)
            self.stdout_writer.write(struct.pack("<I", len(message_bytes)))

            # Write message body
            self.stdout_writer.write(message_bytes)

            await self.stdout_writer.drain()
        except Exception:
            logger.exception("Error writing native message")

    async def handle_extension_message(self, message: dict[str, Any]) -> None:
        """Process messages from browser extension."""
        msg_id = message.get("id")

        if msg_id and msg_id in self.pending_responses:
            # This is a response to a CLI request
            future = self.pending_responses.pop(msg_id)
            future.set_result(message)
        else:
            # This is a command from the extension - shouldn't happen in our architecture
            logger.warning("Unexpected command from extension: %s", message)

    async def handle_cli_client(
        self,
        reader: asyncio.StreamReader,
        writer: asyncio.StreamWriter,
    ) -> None:
        """Handle connection from CLI client."""
        logger.info("CLI client connected")
        client = (reader, writer)
        self.cli_clients.add(client)

        try:
            while True:
                # Read JSON line
                line = await reader.readline()
                if not line:
                    break

                await self.handle_cli_message(writer, line.decode("utf-8").strip())

        except Exception:
            logger.exception("Error handling CLI client")
        finally:
            logger.info("CLI client disconnected")
            self.cli_clients.discard(client)
            writer.close()
            await writer.wait_closed()

    async def handle_cli_message(
        self,
        writer: asyncio.StreamWriter,
        message: str,
    ) -> None:
        """Process messages from CLI client."""
        try:
            data = json.loads(message)

            # Add unique ID for tracking responses
            msg_id = f"cli_{self.message_counter}"
            self.message_counter += 1
            data["id"] = msg_id

            # Create future for response
            future = asyncio.Future()
            self.pending_responses[msg_id] = future

            # Forward to extension via native messaging
            await self.write_native_message(data)

            # Wait for response with timeout
            try:
                response = await asyncio.wait_for(future, timeout=30.0)
                # Send response as JSON line
                writer.write((json.dumps(response) + "\n").encode("utf-8"))
                await writer.drain()
            except TimeoutError:
                self.pending_responses.pop(msg_id, None)
                timeout_response = {
                    "error": "Request timeout",
                    "id": msg_id,
                }
                writer.write((json.dumps(timeout_response) + "\n").encode("utf-8"))
                await writer.drain()

        except json.JSONDecodeError:
            logger.exception("Invalid JSON from CLI: %s", message)
            error_response = {"error": "Invalid JSON"}
            writer.write((json.dumps(error_response) + "\n").encode("utf-8"))
            await writer.drain()

    async def native_messaging_loop(self) -> None:
        """Handle native messaging in a loop."""
        await self.setup_native_messaging()

        while True:
            message = await self.read_native_message()
            if message is None:
                break

            await self.handle_extension_message(message)

    async def start(self) -> None:
        """Start the bridge server."""
        # Get runtime directory for Unix socket
        runtime_dir = os.environ.get("XDG_RUNTIME_DIR")
        if not runtime_dir:
            runtime_dir = tempfile.gettempdir()

        socket_path = Path(runtime_dir) / "browser-cli.sock"

        # Remove old socket if exists
        socket_path.unlink(missing_ok=True)

        # Start Unix socket server for CLI
        server = await asyncio.start_unix_server(
            self.handle_cli_client,
            socket_path,
        )

        # Set socket permissions
        socket_path.chmod(0o600)

        # Send socket path to extension via native messaging
        await self.write_native_message(
            {
                "socket_path": str(socket_path),
                "ready": True,
            },
        )

        logger.info("Native messaging bridge started")
        logger.info("CLI socket: %s", socket_path)

        # Run native messaging loop
        try:
            await self.native_messaging_loop()
        finally:
            # Clean shutdown
            server.close()
            await server.wait_closed()
            socket_path.unlink(missing_ok=True)


async def async_main() -> None:
    """Run the native messaging bridge."""
    bridge = NativeMessagingBridge()

    # Handle shutdown gracefully
    stop = asyncio.Future()

    def signal_handler() -> None:
        stop.set_result(None)

    loop = asyncio.get_event_loop()
    for sig in (signal.SIGTERM, signal.SIGINT):
        loop.add_signal_handler(sig, signal_handler)

    # Run bridge until stopped
    bridge_task = asyncio.create_task(bridge.start())

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
