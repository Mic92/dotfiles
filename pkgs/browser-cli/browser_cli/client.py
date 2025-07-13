"""Browser CLI client - WebSocket client for browser automation."""

import base64
import json
import sys
from pathlib import Path
from typing import Any

import websockets

from browser_cli.errors import BrowserConnectionError, CommandError


class BrowserCLI:
    """WebSocket client for browser automation."""

    def __init__(self, server_url: str = "ws://localhost:9223") -> None:
        """Initialize the browser CLI client."""
        self.server_url = server_url
        self.message_counter = 0

    async def send_command(
        self,
        command: str,
        params: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """Send command to browser and wait for response."""
        self.message_counter += 1
        message_id = str(self.message_counter)

        message = {
            "command": command,
            "params": params or {},
            "id": message_id,
        }

        try:
            async with websockets.connect(self.server_url) as websocket:
                await websocket.send(json.dumps(message))

                # Wait for response with matching ID
                while True:
                    response_str = await websocket.recv()
                    response = json.loads(response_str)

                    if response.get("id") == message_id:
                        if response.get("success"):
                            return response.get("result", {})
                        error_msg = response.get("error", "Unknown error")
                        raise CommandError(error_msg) from None

        except ConnectionRefusedError as e:
            msg = (
                "Cannot connect to browser extension. Make sure:\n"
                "1. Firefox is running\n"
                "2. The Browser CLI extension is installed\n"
                "3. The extension is enabled on the current tab\n"
                "4. The WebSocket server is running (browser-cli-server)"
            )
            raise BrowserConnectionError(msg) from e

    async def navigate(self, url: str) -> None:
        """Navigate to URL."""
        result = await self.send_command("navigate", {"url": url})
        print(result.get("message", "Navigated"))

    async def back(self) -> None:
        """Go back in browser history."""
        result = await self.send_command("back")
        print(result.get("message", "Went back"))

    async def forward(self) -> None:
        """Go forward in browser history."""
        result = await self.send_command("forward")
        print(result.get("message", "Went forward"))

    async def click(self, selector: str) -> None:
        """Click an element."""
        result = await self.send_command("click", {"element": selector})
        print(result.get("message", f"Clicked {selector}"))

    async def type_text(self, selector: str, text: str) -> None:
        """Type text into an element."""
        result = await self.send_command("type", {"element": selector, "text": text})
        print(result.get("message", f"Typed into {selector}"))

    async def hover(self, selector: str) -> None:
        """Hover over an element."""
        result = await self.send_command("hover", {"element": selector})
        print(result.get("message", f"Hovered over {selector}"))

    async def drag(self, start_selector: str, end_selector: str) -> None:
        """Drag from one element to another."""
        result = await self.send_command(
            "drag",
            {
                "startElement": start_selector,
                "endElement": end_selector,
            },
        )
        print(result.get("message", f"Dragged from {start_selector} to {end_selector}"))

    async def select(self, selector: str, option: str) -> None:
        """Select an option in a dropdown."""
        result = await self.send_command("select", {"element": selector, "option": option})
        print(result.get("message", f"Selected {option} in {selector}"))

    async def wait(self, seconds: float) -> None:
        """Wait for specified seconds."""
        result = await self.send_command("wait", {"seconds": seconds})
        print(result.get("message", f"Waited {seconds} seconds"))

    async def key(self, key: str) -> None:
        """Press a keyboard key."""
        result = await self.send_command("key", {"key": key})
        print(result.get("message", f"Pressed key: {key}"))

    async def screenshot(self, output_file: str | None = None) -> None:
        """Take a screenshot."""
        result = await self.send_command("screenshot")

        if "screenshot" in result:
            # Extract base64 data from data URL
            data_url = result["screenshot"]
            if data_url.startswith("data:image/png;base64,"):
                base64_data = data_url.split(",")[1]
                image_data = base64.b64decode(base64_data)

                # Save to file
                output_path = Path(output_file) if output_file else Path("screenshot.png")

                output_path.write_bytes(image_data)
                print(f"Screenshot saved to {output_path}")
            else:
                print("Error: Invalid screenshot data", file=sys.stderr)
        else:
            print("Error: No screenshot data received", file=sys.stderr)

    async def console(self) -> None:
        """Get console logs from the page."""
        result = await self.send_command("console")

        if "logs" in result:
            logs = result["logs"]
            if not logs:
                print("No console logs")
            else:
                for log in logs:
                    log_type = log.get("type", "log").upper()
                    message = log.get("message", "")
                    timestamp = log.get("timestamp", "")
                    print(f"[{timestamp}] {log_type}: {message}")
        else:
            print("No console logs available")

    async def snapshot(self) -> None:
        """Get ARIA snapshot of the page."""
        result = await self.send_command("snapshot")

        if "snapshot" in result:
            snapshot = result["snapshot"]
            if not snapshot:
                print("Empty snapshot")
            else:
                self._print_snapshot(snapshot)
        else:
            print("No snapshot available")

    def _print_snapshot(self, nodes: list, indent: int = 0) -> None:
        """Print ARIA snapshot in a readable format."""
        for node in nodes:
            prefix = "  " * node.get("level", indent)

            if node.get("type") == "text":
                content = node.get("content", "").strip()
                if content:
                    print(f"{prefix}{content}")
            else:
                role = node.get("role", "")
                label = node.get("label", "")
                attrs = node.get("attributes", {})

                # Build element description
                parts = [role]
                if label:
                    parts.append(f'"{label}"')

                # Add relevant attributes
                if attrs.get("href"):
                    parts.append(f'href="{attrs["href"]}"')
                if attrs.get("value"):
                    parts.append(f'value="{attrs["value"]}"')
                if attrs.get("type"):
                    parts.append(f'type="{attrs["type"]}"')
                if attrs.get("clickable"):
                    parts.append("[clickable]")

                print(f"{prefix}<{' '.join(parts)}>")
