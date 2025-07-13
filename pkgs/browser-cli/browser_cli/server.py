#!/usr/bin/env python3
"""WebSocket server that acts as a bridge between the browser extension and CLI.

The browser extension spawns this server and connects to it.
"""

import asyncio
import json
import logging
import logging.handlers
import os
import signal
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


class BrowserBridge:
    """Bridge between browser extension and CLI clients."""

    def __init__(self) -> None:
        """Initialize the browser bridge."""
        self.extension_client: WebSocketServerProtocol | None = None
        self.cli_clients: set[WebSocketServerProtocol] = set()
        self.pending_responses: dict[str, asyncio.Future] = {}
        self.message_counter = 0

    async def handle_extension(
        self,
        websocket: WebSocketServerProtocol,
    ) -> None:
        """Handle connection from browser extension."""
        logger.info("Browser extension connected")
        self.extension_client = websocket
        try:
            # Read message length (4 bytes, little-endian)
            length_bytes = await self.stdin_reader.readexactly(4)
            length = struct.unpack("<I", length_bytes)[0]
            
            # Read message body
            message_bytes = await self.stdin_reader.readexactly(length)
            message = json.loads(message_bytes.decode("utf-8"))
            
            return message
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
        self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter
    ) -> None:
        """Handle connection from CLI client."""
        logger.info("CLI client connected")
        self.cli_clients.add(websocket)
        try:
            async for message in websocket:
                await self.handle_cli_message(websocket, message)
        except websockets.exceptions.ConnectionClosed:
            logger.info("CLI client disconnected")
        finally:
            self.cli_clients.discard(websocket)

    async def handle_extension_message(self, message: str) -> None:
        """Process messages from browser extension."""
        try:
            data = json.loads(message)
            msg_id = data.get("id")

            if msg_id and msg_id in self.pending_responses:
                # This is a response to a CLI request
                future = self.pending_responses.pop(msg_id)
                future.set_result(data)
            else:
                # This is an event from the extension
                await self.broadcast_to_cli(data)
        except json.JSONDecodeError:
            logger.exception("Invalid JSON from extension: %s", message)

    async def handle_cli_message(
        self,
        websocket: WebSocketServerProtocol,
        message: str,
    ) -> None:
        """Process messages from CLI client."""
        try:
            data = json.loads(message)

            if not self.extension_client:
                error_response = {
                    "error": "Browser extension not connected",
                    "id": data.get("id"),
                }
                await websocket.send(json.dumps(error_response))
                return

            # Add unique ID for tracking responses
            msg_id = f"cli_{self.message_counter}"
            self.message_counter += 1
            data["id"] = msg_id

            # Create future for response
            future = asyncio.Future()
            self.pending_responses[msg_id] = future

            # Forward to extension
            await self.extension_client.send(json.dumps(data))

            # Wait for response with timeout
            try:
                response = await asyncio.wait_for(future, timeout=30.0)
                await websocket.send(json.dumps(response))
            except TimeoutError:
                self.pending_responses.pop(msg_id, None)
                timeout_response = {
                    "error": "Request timeout",
                    "id": msg_id,
                }
                await websocket.send(json.dumps(timeout_response))

        except json.JSONDecodeError:
            logger.exception("Invalid JSON from CLI: %s", message)
            error_response = {"error": "Invalid JSON"}
            await websocket.send(json.dumps(error_response))

    async def broadcast_to_cli(self, data: dict[str, Any]) -> None:
        """Broadcast events to all connected CLI clients."""
        if self.cli_clients:
            message = json.dumps(data)
            disconnected = set()
            for client in self.cli_clients:
                try:
                    await client.send(message)
                except websockets.exceptions.ConnectionClosed:
                    disconnected.add(client)
            self.cli_clients -= disconnected


async def async_main() -> None:
    """Start the WebSocket bridge server."""
    bridge = BrowserBridge()

    # Start two servers on different ports
    extension_server = await websockets.serve(
        bridge.handle_extension,
        "localhost",
        9222,
    )
    cli_server = await websockets.serve(
        bridge.handle_cli,
        "localhost",
        9223,
    )

    logger.info("WebSocket bridge started")
    logger.info("Extension port: 9222")
    logger.info("CLI port: 9223")

    # Handle shutdown gracefully
    stop = asyncio.Future()

    def signal_handler() -> None:
        stop.set_result(None)

    loop = asyncio.get_event_loop()
    for sig in (signal.SIGTERM, signal.SIGINT):
        loop.add_signal_handler(sig, signal_handler)

    await stop

    # Clean shutdown
    extension_server.close()
    cli_server.close()
    await extension_server.wait_closed()
    await cli_server.wait_closed()


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
