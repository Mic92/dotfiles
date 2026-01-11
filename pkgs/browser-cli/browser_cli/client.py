"""Browser CLI client - Unix socket client for browser automation."""

import asyncio
import json
import os
import tempfile
from pathlib import Path
from typing import Any

from browser_cli.errors import BrowserConnectionError, CommandError


class BrowserClient:
    """Unix socket client for browser automation."""

    def __init__(self, socket_path: str | None = None) -> None:
        """Initialize the browser CLI client."""
        if socket_path:
            self.socket_path = socket_path
        else:
            runtime_dir = os.environ.get("XDG_RUNTIME_DIR")
            if not runtime_dir:
                runtime_dir = tempfile.gettempdir()
            self.socket_path = str(Path(runtime_dir) / "browser-cli.sock")

        self.message_counter = 0

    async def send_command(
        self,
        command: str,
        params: dict[str, Any] | None = None,
        tab_id: str | None = None,
    ) -> dict[str, Any]:
        """Send command to browser and wait for response."""
        self.message_counter += 1
        message_id = str(self.message_counter)

        message: dict[str, Any] = {
            "command": command,
            "params": params or {},
            "id": message_id,
        }

        if tab_id:
            message["tabId"] = tab_id

        try:
            reader, writer = await asyncio.open_unix_connection(
                self.socket_path,
                limit=10 * 1024 * 1024,  # 10MB for screenshots
            )

            writer.write((json.dumps(message) + "\n").encode("utf-8"))
            await writer.drain()

            response_line = await reader.readline()
            if not response_line:
                msg = "No response from server"
                raise CommandError(msg)

            response = json.loads(response_line.decode("utf-8").strip())

            writer.close()
            await writer.wait_closed()

            if response.get("id") == message_id:
                if response.get("success"):
                    result: dict[str, Any] = response.get("result", {})
                    return result
                error_msg = response.get("error", "Unknown error")
                raise CommandError(error_msg) from None

            msg = f"Invalid response ID. Expected: {message_id}, Got: {response.get('id')}"
            raise CommandError(msg)

        except (ConnectionRefusedError, FileNotFoundError) as e:
            msg = (
                "Cannot connect to browser extension. Make sure:\n"
                "1. Firefox is running\n"
                "2. The Browser CLI extension is installed\n"
                "3. The extension is enabled on the current tab\n"
                f"\nSocket path: {self.socket_path}"
            )
            raise BrowserConnectionError(msg) from e

    async def exec_js(
        self,
        code: str,
        tab_id: str | None = None,
    ) -> dict[str, object] | list[object] | str | int | float | bool | None:
        """Execute JavaScript code in the browser and return result."""
        result = await self.send_command("exec", {"code": code}, tab_id)
        js_result: dict[str, object] | list[object] | str | int | float | bool | None = result.get(
            "result",
        )
        return js_result

    async def list_tabs(self) -> list[dict[str, Any]]:
        """List all managed tabs."""
        result = await self.send_command("list-tabs")
        tabs: list[dict[str, Any]] = result.get("tabs", [])
        return tabs
