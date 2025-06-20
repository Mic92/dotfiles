"""Tmux MCP Server - provides tmux integration for Claude Code."""

import asyncio
import logging
import signal
import sys
from typing import Any

from anyio import BrokenResourceError
from mcp.server import Server
from mcp.server.models import InitializationOptions
from mcp.server.stdio import stdio_server
from mcp.types import ServerCapabilities, TextContent, Tool, ToolsCapability

from .commands import (
    capture_pane_tool,
    get_command_output_tool,
    kill_pane_tool,
    list_panes_tool,
    list_sessions_tool,
    ripgrep_command_output_tool,
    run_command_tool,
    send_input_tool,
    tmux_capture_pane,
    tmux_get_command_output,
    tmux_kill_pane,
    tmux_list_panes,
    tmux_list_sessions,
    tmux_ripgrep_command_output,
    tmux_run_command,
    tmux_send_input,
)
from .commands.base import CommandResult, SimpleResult

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("tmux-mcp")

# Create the MCP server
server: Server[Any] = Server("tmux-mcp")


@server.list_tools()  # type: ignore[misc,no-untyped-call]
async def handle_list_tools() -> list[Tool]:
    """List available tmux tools."""
    return [
        run_command_tool,
        send_input_tool,
        list_sessions_tool,
        list_panes_tool,
        kill_pane_tool,
        capture_pane_tool,
        get_command_output_tool,
        ripgrep_command_output_tool,
    ]


async def _handle_tool_call(  # noqa: PLR0911
    name: str, arguments: dict[str, Any]
) -> CommandResult | SimpleResult:
    """Handle individual tool calls and return result."""
    match name:
        case "run_command":
            command = arguments["command"]
            working_dir = arguments.get("working_dir")
            session_name = arguments.get("session_name")
            timeout_seconds = arguments.get("timeout", 300)
            keep_pane = arguments.get("keep_pane", False)
            window_name = arguments.get("window_name")
            expect_pattern = arguments.get("expect_pattern")
            return await tmux_run_command(
                command,
                working_dir,
                session_name,
                timeout_seconds,
                keep_pane,
                window_name,
                expect_pattern,
            )

        case "send_input":
            pane_id = arguments["pane_id"]
            input_text = arguments["input_text"]
            expect_before = arguments.get("expect_before")
            expect_after = arguments.get("expect_after")
            timeout_seconds = arguments.get("timeout", 30)
            return await tmux_send_input(
                pane_id, input_text, expect_before, expect_after, timeout_seconds
            )

        case "list_sessions":
            output = await tmux_list_sessions()
            return SimpleResult(output=output)

        case "list_panes":
            session_name = arguments.get("session_name")
            output = await tmux_list_panes(session_name)
            return SimpleResult(output=output)

        case "kill_pane":
            pane_id = arguments["pane_id"]
            output = await tmux_kill_pane(pane_id)
            return SimpleResult(output=output)

        case "capture_pane":
            pane_id = arguments["pane_id"]
            start_line = arguments.get("start_line")
            expect_pattern = arguments.get("expect_pattern")
            timeout_seconds = arguments.get("timeout", 30)
            return await tmux_capture_pane(
                pane_id, start_line, expect_pattern, timeout_seconds
            )

        case "get_command_output":
            pane_id = arguments["pane_id"]
            cursor = arguments.get("cursor")
            return await tmux_get_command_output(pane_id, cursor)

        case "ripgrep_command_output":
            pane_id = arguments["pane_id"]
            pattern = arguments["pattern"]
            flags = arguments.get("flags")
            cursor = arguments.get("cursor")
            return await tmux_ripgrep_command_output(pane_id, pattern, flags, cursor)

        case _:
            return CommandResult(
                pane_id="",
                exit_code=-1,
                output="",
                command="",
                error=f"Unknown tool: {name}",
            )


@server.call_tool()  # type: ignore[misc,no-untyped-call]
async def handle_call_tool(name: str, arguments: dict[str, Any]) -> list[TextContent]:
    """Handle tool calls by delegating to specific tmux functions."""
    result = await _handle_tool_call(name, arguments)

    # Format output based on result type
    if isinstance(result, CommandResult):
        output_parts = []

        # Add command info if present
        if result.command:
            output_parts.append(f"Command: {result.command}")
        if result.working_dir:
            output_parts.append(f"Working Directory: {result.working_dir}")
        output_parts.append(f"Exit Code: {result.exit_code}")

        # Add output
        if result.output:
            output_parts.append(f"\nOutput:\n{result.output}")

        # Add error if present
        if result.error:
            output_parts.append(f"\nError: {result.error}")

        # Add pane ID for reference
        if result.pane_id:
            output_parts.append(f"\nPane ID: {result.pane_id}")

        # Add pagination info if present
        if result.pagination:
            output_parts.append(
                f"\nPagination: Showing lines {result.pagination.start_line + 1}-"
                f"{result.pagination.start_line + result.pagination.displayed_lines} "
                f"of {result.pagination.total_lines} total"
            )
            if result.pagination.next_cursor:
                output_parts.append(f"Next cursor: {result.pagination.next_cursor}")

        text = "\n".join(output_parts)
    else:
        # SimpleResult
        text = result.output

    return [TextContent(type="text", text=text)]


async def _run_server() -> None:
    """Run the server with stdio transport."""
    try:
        async with stdio_server() as (read_stream, write_stream):
            await server.run(
                read_stream,
                write_stream,
                InitializationOptions(
                    server_name="tmux-mcp",
                    server_version="0.1.1",
                    capabilities=ServerCapabilities(tools=ToolsCapability()),
                ),
            )
    except BrokenResourceError:
        logger.info("Connection closed by client")
    except Exception:
        logger.exception("Server error")
        raise


def handle_signals() -> None:
    """Set up signal handlers for graceful shutdown."""

    def signal_handler(signum: int, frame: Any) -> None:
        logger.info(f"Received signal {signum}, shutting down gracefully...")
        sys.exit(0)

    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)


def main() -> None:
    """Run the MCP server using stdin/stdout."""
    logger.info("Starting Tmux MCP Server...")
    handle_signals()

    try:
        asyncio.run(_run_server())
    except KeyboardInterrupt:
        logger.info("Server stopped by user")
    except BrokenResourceError:
        logger.info("Connection closed")
    except ExceptionGroup as eg:
        # Handle ExceptionGroup from anyio TaskGroup
        for exc in eg.exceptions:
            if isinstance(exc, BrokenResourceError):
                logger.info("Connection closed during task group operation")
            else:
                logger.exception("Error in task group")
    except Exception:
        logger.exception("Server crashed")
        sys.exit(1)


if __name__ == "__main__":
    main()
