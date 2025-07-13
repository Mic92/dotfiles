"""Browser CLI client - Unix socket client for browser automation."""

import asyncio
import json
import os
import sys
import tempfile
from pathlib import Path
from typing import Any

from browser_cli.errors import BrowserConnectionError, CommandError


class BrowserCLI:
    """Unix socket client for browser automation."""

    def __init__(self, socket_path: str | None = None) -> None:
        """Initialize the browser CLI client."""
        if socket_path:
            self.socket_path = socket_path
        else:
            # Use default socket path
            runtime_dir = os.environ.get("XDG_RUNTIME_DIR")
            if not runtime_dir:
                runtime_dir = tempfile.gettempdir()
            self.socket_path = str(Path(runtime_dir) / "browser-cli.sock")

        self.message_counter = 0
        self.current_tab: str | None = None

    async def send_command(
        self,
        command: str,
        params: dict[str, Any] | None = None,
        tab_id: str | None = None,
    ) -> dict[str, Any]:
        """Send command to browser and wait for response."""
        self.message_counter += 1
        message_id = str(self.message_counter)

        message = {
            "command": command,
            "params": params or {},
            "id": message_id,
        }

        # Add tab ID if specified
        if tab_id or self.current_tab:
            message["tabId"] = tab_id or self.current_tab or ""

        try:
            # Connect to Unix socket
            reader, writer = await asyncio.open_unix_connection(self.socket_path)

            # Send command as JSON line
            writer.write((json.dumps(message) + "\n").encode("utf-8"))
            await writer.drain()

            # Read response line
            response_line = await reader.readline()
            if not response_line:
                msg = "No response from server"
                raise CommandError(msg)

            response = json.loads(response_line.decode("utf-8").strip())

            # Close connection
            writer.close()
            await writer.wait_closed()

            # Check response
            if response.get("id") == message_id:
                if response.get("success"):
                    result: dict[str, Any] = response.get("result", {})
                    return result
                error_msg = response.get("error", "Unknown error")
                raise CommandError(error_msg) from None
            msg = (
                f"Invalid response ID. Expected: {message_id}, "
                f"Got: {response.get('id', 'None')}. "
                f"Full response: {response}"
            )
            raise CommandError(msg)

        except (ConnectionRefusedError, FileNotFoundError) as e:
            msg = (
                "Cannot connect to browser extension. Make sure:\n"
                "1. Firefox is running\n"
                "2. The Browser CLI extension is installed\n"
                "3. The extension is enabled on the current tab\n"
                "4. The native messaging bridge is running (browser-cli-server)\n"
                f"\nSocket path: {self.socket_path}"
            )
            raise BrowserConnectionError(msg) from e

    def set_tab(self, tab_id: str | None) -> None:
        """Set the current tab for subsequent commands."""
        self.current_tab = tab_id

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

    async def click(self, selector: str, selector_type: str = "css") -> None:
        """Click an element."""
        result = await self.send_command(
            "click",
            {"element": selector, "selectorType": selector_type},
        )
        print(result.get("message", f"Clicked {selector}"))

    async def type_text(self, selector: str, text: str, selector_type: str = "css") -> None:
        """Type text into an element."""
        result = await self.send_command(
            "type",
            {"element": selector, "text": text, "selectorType": selector_type},
        )
        print(result.get("message", f"Typed into {selector}"))

    async def hover(self, selector: str, selector_type: str = "css") -> None:
        """Hover over an element."""
        result = await self.send_command(
            "hover",
            {"element": selector, "selectorType": selector_type},
        )
        print(result.get("message", f"Hovered over {selector}"))

    async def drag(
        self,
        start_selector: str,
        end_selector: str,
        selector_type: str = "css",
    ) -> None:
        """Drag from one element to another."""
        result = await self.send_command(
            "drag",
            {
                "startElement": start_selector,
                "endElement": end_selector,
                "selectorType": selector_type,
            },
        )
        print(result.get("message", f"Dragged from {start_selector} to {end_selector}"))

    async def select(self, selector: str, option: str, selector_type: str = "css") -> None:
        """Select an option in a dropdown."""
        result = await self.send_command(
            "select",
            {"element": selector, "option": option, "selectorType": selector_type},
        )
        print(result.get("message", f"Selected {option} in {selector}"))

    async def key(self, key: str) -> None:
        """Press a keyboard key."""
        result = await self.send_command("key", {"key": key})
        print(result.get("message", f"Pressed key: {key}"))

    async def screenshot(self, output_file: str | None = None) -> None:
        """Take a screenshot."""
        # Determine output path
        output_path = Path(output_file).absolute() if output_file else Path.cwd() / "screenshot.png"

        # Send command with desired output path
        result = await self.send_command("screenshot", {"output_path": str(output_path)})

        if "screenshot_path" in result:
            # Server saved the screenshot to the requested path
            print(f"Screenshot saved to {result['screenshot_path']}")
        elif "message" in result:
            print(result["message"])
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

    async def snapshot(self, offset: int | None = None, limit: int | None = None) -> None:
        """Get ARIA snapshot of the page."""
        params = {}
        if offset is not None:
            params["offset"] = offset
        if limit is not None:
            params["limit"] = limit

        result = await self.send_command("snapshot", params)

        if "snapshot" in result:
            snapshot = result["snapshot"]
            total_nodes = result.get("totalNodes", 0)

            if not snapshot:
                print("Empty snapshot")
            else:
                # Show pagination info if applicable
                if offset is not None or limit is not None:
                    current_offset = offset or 0
                    shown = len(snapshot)
                    print(
                        f"=== Showing nodes {current_offset + 1}-{current_offset + shown} "
                        f"of {total_nodes} ===",
                    )
                    if current_offset + shown < total_nodes:
                        print(f"=== Use --offset {current_offset + shown} to see more ===")
                    print()

                self._print_snapshot(snapshot)

                # Show navigation hint at the bottom too for large outputs
                if (
                    limit is not None
                    and len(snapshot) == limit
                    and current_offset + shown < total_nodes
                ):
                    print()
                    print(
                        f"=== Showing nodes {current_offset + 1}-{current_offset + shown} "
                        f"of {total_nodes} ===",
                    )
                    print(f"=== Use --offset {current_offset + shown} to see more ===")
        else:
            print("No snapshot available")

    def _print_snapshot(self, nodes: list[dict[str, Any]], indent: int = 0) -> None:
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

    async def list_tabs(self) -> None:
        """List all tabs managed by the extension."""
        result = await self.send_command("list-tabs")

        if "tabs" in result:
            tabs = result["tabs"]
            if not tabs:
                print("No managed tabs")
            else:
                print("Managed tabs:")
                for tab in tabs:
                    tab_id = tab.get("id", "unknown")
                    url = tab.get("url", "about:blank")
                    title = tab.get("title", "Untitled")
                    active = " (active)" if tab.get("active") else ""
                    print(f"  {tab_id}: {title}{active}")
                    print(f"       {url}")
        else:
            print("No tabs information available")

    async def new_tab(self, url: str | None = None) -> None:
        """Create a new tab managed by the extension."""
        params = {}
        if url:
            params["url"] = url

        result = await self.send_command("new-tab", params)

        if "tabId" in result:
            tab_id = result["tabId"]
            print(f"Created new tab: {tab_id}")
            if "url" in result:
                print(f"Opened: {result['url']}")
        else:
            print(result.get("message", "Tab created"))

    async def eval(self, expression: str) -> None:
        """Evaluate JavaScript expression and return result as JSON."""
        result = await self.send_command("eval", {"expression": expression})

        if "result" in result:
            # Pretty print the JSON result
            print(json.dumps(result["result"], indent=2))
        else:
            print(result.get("message", "No result returned"))
