"""Tmux MCP Server - provides tmux integration for Claude Code."""

import asyncio
import contextlib
import logging
import os
import shlex
import subprocess
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import anyio
from mcp.server import Server
from mcp.server.stdio import stdio_server
from mcp.types import (
    TextContent,
    Tool,
)


class TmuxError(Exception):
    """Custom exception for tmux-related errors."""


class TmuxCommandError(TmuxError):
    """Exception raised for errors in tmux commands."""


@dataclass
class PaginationInfo:
    """Pagination information for command output."""

    total_lines: int
    displayed_lines: int
    start_line: int
    next_cursor: str | None = None


@dataclass
class CommandResult:
    """Result of a tmux command execution."""

    pane_id: str
    exit_code: int
    output: str
    command: str
    working_dir: str | None = None
    error: str | None = None
    pagination: PaginationInfo | None = None


@dataclass
class SimpleResult:
    """Simple result with just output."""

    output: str


# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("tmux-mcp")

# Global cache for command outputs
# Maps pane_id -> output_file_path
_output_cache: dict[str, Path] = {}
# Cache cleanup registry
_cache_cleanup_tasks: dict[str, asyncio.Task[None]] = {}


def run_tmux_command(args: list[str]) -> str:
    """Run a tmux command and return the output."""
    try:
        result = subprocess.run(
            ["tmux", *args], capture_output=True, text=True, check=True, timeout=30
        )
        return result.stdout.strip()
    except subprocess.CalledProcessError as e:
        error_msg = f"Tmux command failed: {e.stderr}"
        raise TmuxCommandError(error_msg) from e
    except subprocess.TimeoutExpired as e:
        error_msg = "Tmux command timed out"
        raise TmuxCommandError(error_msg) from e


def _get_target_session(session_name: str | None) -> str:
    """Get the target tmux session, creating 'mcp' session if needed."""
    if session_name:
        return session_name

    # Use current session if available, otherwise create a default one
    try:
        return run_tmux_command(["display-message", "-p", "#{session_name}"])
    except TmuxError:
        # Not in a tmux session, create or use default session
        try:
            run_tmux_command(["has-session", "-t", "mcp"])
        except TmuxError:
            run_tmux_command(["new-session", "-d", "-s", "mcp"])
        return "mcp"


def _build_shell_command(
    command: str,
    working_dir: str | None,
    completion_fifo: Path,
    output_file: Path,
) -> str:
    """Build the shell command that will run in tmux with FIFO synchronization."""
    # Don't quote the command since it will be inside the shell script
    quoted_completion_fifo = shlex.quote(str(completion_fifo))
    quoted_output_file = shlex.quote(str(output_file))
    quoted_working_dir = shlex.quote(working_dir or "")

    # Command structure:
    # 1. Run the actual command directly
    # 2. Use tee to live stream output to both terminal and file
    # 3. Capture exit code
    # 4. Write completion signal to FIFO (this unblocks our async wait)
    script_content = f"""
# Change to working directory if specified
{f"cd {quoted_working_dir} &&" if working_dir else ""}

# Run command with tee to live stream output and capture to file
{command} 2>&1 | tee {quoted_output_file}
exit_code=${{PIPESTATUS[0]}}

# Signal completion by writing to FIFO
echo "done:$exit_code" > {quoted_completion_fifo}
"""

    # Return the bash command that executes our script
    return f"bash -c {shlex.quote(script_content)}"


def _create_tmux_window(
    target_session: str, working_dir: str | None, shell_command: str
) -> str:
    """Create a new tmux window and return its unique name."""
    import random

    window_name = f"cmd-{int(time.time())}-{random.randint(1000, 9999)}"  # noqa: S311

    # First create the window with a shell
    window_args = [
        "new-window",
        "-d",  # detached
        "-t",
        target_session + ":",  # auto-assign window index
        "-n",
        window_name,
    ]

    if working_dir:
        window_args.extend(["-c", working_dir])

    # Create window with bash shell
    window_args.append("bash")
    run_tmux_command(window_args)

    # Now send the actual command to the window
    run_tmux_command(
        ["send-keys", "-t", f"{target_session}:{window_name}", shell_command, "Enter"]
    )

    return window_name


def _find_pane_id(target_session: str, window_name: str) -> str:
    """Find the pane ID of a created window."""
    windows_output = run_tmux_command(
        [
            "list-windows",
            "-t",
            target_session,
            "-F",
            "#{window_name}:#{pane_id}:#{window_activity}",
        ]
    )

    for line in windows_output.strip().split("\n"):
        if line and line.startswith(f"{window_name}:"):
            parts = line.rsplit(":", 2)
            if len(parts) >= 3:
                _, pane_id_found, _ = parts
                return pane_id_found

    msg = "Could not find created window pane"
    raise TmuxError(msg)


async def _wait_for_completion(completion_fifo: Path) -> tuple[bool, int]:
    """Wait for completion signal. Returns (success, exit_code)."""
    logger.debug(f"Waiting for completion signal from FIFO: {completion_fifo}")

    proc = None
    try:
        # Use asyncio subprocess to read from FIFO instead of threads
        proc = await asyncio.create_subprocess_exec(
            "cat",
            str(completion_fifo),
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )

        stdout, stderr = await proc.communicate()
        signal = stdout.decode().strip()

        logger.debug(f"Read signal from FIFO: {signal!r}")

        if signal.startswith("done:"):
            exit_code = int(signal.split(":", 1)[1].strip())
            logger.debug(f"Parsed exit code: {exit_code}")
            return True, exit_code
    except asyncio.CancelledError:
        logger.debug("Completion wait was cancelled")
        if proc:
            with contextlib.suppress(Exception):
                proc.terminate()
                await proc.wait()
        raise
    except (OSError, ValueError) as e:
        logger.debug(f"Error processing signal: {e}")

    logger.debug("Returning failure from _wait_for_completion")
    return False, -1


def _parse_cursor(cursor: str | None) -> tuple[int, int]:
    """Parse pagination cursor into start_line and max_lines.

    Cursor format: "start_line:max_lines" or empty for no pagination.
    Returns (start_line, max_lines) where 0 means no limit.
    """
    if not cursor:
        return 0, 0

    try:
        parts = cursor.split(":")
        if len(parts) == 2:
            start_line = int(parts[0])
            max_lines = int(parts[1])
            return max(0, start_line), max(0, max_lines)
    except ValueError:
        pass

    return 0, 0


def _create_next_cursor(
    start_line: int, max_lines: int, total_lines: int
) -> str | None:
    """Create next cursor if there are more lines to display."""
    if max_lines == 0:  # No pagination
        return None

    next_start = start_line + max_lines
    if next_start >= total_lines:
        return None

    return f"{next_start}:{max_lines}"


async def _cleanup_cached_output(pane_id: str, delay_seconds: int = 300) -> None:
    """Clean up cached output after a delay."""
    try:
        await asyncio.sleep(delay_seconds)
        if pane_id in _output_cache:
            output_file = _output_cache[pane_id]
            try:
                if output_file.exists():
                    output_file.unlink()
                    logger.debug(f"Cleaned up cached output file: {output_file}")
            except OSError as e:
                logger.debug(f"Failed to clean up output file {output_file}: {e}")
            finally:
                _output_cache.pop(pane_id, None)
                _cache_cleanup_tasks.pop(pane_id, None)
    except asyncio.CancelledError:
        # Task was cancelled, still clean up
        if pane_id in _output_cache:
            _output_cache.pop(pane_id, None)
        _cache_cleanup_tasks.pop(pane_id, None)
        raise


def _cache_output_file(pane_id: str, output_file: Path) -> None:
    """Cache an output file for later pagination."""
    # Cancel any existing cleanup task for this pane (safely)
    if pane_id in _cache_cleanup_tasks:
        with contextlib.suppress(RuntimeError):
            # Event loop might be closed - ignore cancellation errors
            _cache_cleanup_tasks[pane_id].cancel()
        _cache_cleanup_tasks.pop(pane_id, None)

    # Store the output file path
    _output_cache[pane_id] = output_file

    # Schedule cleanup after 5 minutes (only if event loop is running)
    try:
        loop = asyncio.get_running_loop()
        if not loop.is_closed():
            cleanup_task = asyncio.create_task(_cleanup_cached_output(pane_id))
            _cache_cleanup_tasks[pane_id] = cleanup_task
    except RuntimeError:
        # No running event loop - skip cleanup task scheduling
        pass


async def _read_command_output(
    output_file: Path, cursor: str | None = None
) -> tuple[str, PaginationInfo]:
    """Read the output file from the executed command with pagination support.

    Args:
        output_file: Path to the output file
        cursor: Pagination cursor in format "start_line:max_lines"

    Returns:
        tuple: (output_content, pagination_info)
        pagination_info contains MCP-style pagination with next_cursor
    """
    try:
        if output_file.exists():
            async with await anyio.open_file(output_file, "r") as f:
                lines = await f.readlines()

            total_lines = len(lines)
            start_line, max_lines = _parse_cursor(cursor)

            # Apply pagination if specified
            if max_lines > 0:
                end_line = start_line + max_lines
                displayed_lines = lines[start_line:end_line]
                next_cursor = _create_next_cursor(start_line, max_lines, total_lines)
            else:
                # No pagination - return all lines from start_line
                displayed_lines = lines[start_line:] if start_line > 0 else lines
                next_cursor = None

            output_content = "".join(displayed_lines)

            pagination_info = PaginationInfo(
                total_lines=total_lines,
                displayed_lines=len(displayed_lines),
                start_line=start_line,
                next_cursor=next_cursor,
            )

            return output_content, pagination_info
    except OSError:
        pass

    return "Failed to read command output", PaginationInfo(
        total_lines=0,
        displayed_lines=0,
        start_line=0,
    )


async def tmux_run_command(
    command: str,
    working_dir: str | None = None,
    session_name: str | None = None,
    timeout_seconds: int = 300,
    keep_pane: bool = False,
) -> CommandResult:
    """Run a command in a new tmux pane and wait for completion using FIFO synchronization."""
    logger.debug(
        f"Starting tmux_run_command: command={command!r}, working_dir={working_dir!r}, session_name={session_name!r}, timeout={timeout_seconds}"
    )

    try:
        target_session = _get_target_session(session_name)
        logger.debug(f"Target session: {target_session}")

        # Create temporary files for synchronization
        # Note: We create output file outside temp directory so we can cache it
        temp_dir = tempfile.mkdtemp()
        temp_dir_path = Path(temp_dir)
        completion_fifo = temp_dir_path / "completion.fifo"

        # Create output file in system temp with unique name
        with tempfile.NamedTemporaryFile(
            suffix=".txt", prefix="tmux_output_", delete=False
        ) as temp_file:
            output_file = Path(temp_file.name)
        logger.debug(
            f"Created temp dir: {temp_dir}, FIFO: {completion_fifo}, output: {output_file}"
        )

        try:
            # Create FIFO for completion signaling
            os.mkfifo(str(completion_fifo))
            logger.debug(f"Created FIFO: {completion_fifo}")

            # Build and execute the command
            shell_command = _build_shell_command(
                command, working_dir, completion_fifo, output_file
            )
            logger.debug(f"Built shell command: {shell_command}")

            window_name = _create_tmux_window(
                target_session, working_dir, shell_command
            )
            logger.debug(f"Created window: {window_name}")

            pane_id = _find_pane_id(target_session, window_name)
            logger.debug(f"Found pane ID: {pane_id}")

            # Wait for completion with timeout
            logger.debug(
                f"Starting to wait for completion with timeout: {timeout_seconds}s"
            )
            completion_task = asyncio.create_task(_wait_for_completion(completion_fifo))
            try:
                success, exit_code = await asyncio.wait_for(
                    completion_task, timeout=timeout_seconds
                )
                logger.debug(
                    f"Completion wait finished: success={success}, exit_code={exit_code}"
                )
            except TimeoutError:
                logger.debug(f"Command timed out after {timeout_seconds} seconds")
                # Cancel the completion task to clean up subprocess
                completion_task.cancel()
                with contextlib.suppress(asyncio.CancelledError):
                    await completion_task

                # Kill the pane since it timed out (unless keep_pane is True)
                if not keep_pane:
                    try:
                        run_tmux_command(["kill-pane", "-t", pane_id])
                        logger.debug(f"Killed pane {pane_id} after timeout")
                    except TmuxError as e:
                        logger.debug(
                            f"Failed to kill pane {pane_id} after timeout: {e}"
                        )

                return CommandResult(
                    pane_id=pane_id,
                    exit_code=-1,
                    output=f"Command timed out after {timeout_seconds} seconds",
                    command=command,
                    working_dir=working_dir,
                    error="timeout",
                )

            if not success:
                return CommandResult(
                    pane_id=pane_id,
                    exit_code=-1,
                    output="Failed to receive completion signal",
                    command=command,
                    working_dir=working_dir,
                    error="completion_signal_failed",
                )

            # Read the output for initial response (first 100 lines by default)
            output, pagination_info = await _read_command_output(output_file, "0:100")

            # Cache the output file for pagination
            _cache_output_file(pane_id, output_file)

            # Kill the pane now that the command is complete (unless keep_pane is True)
            if not keep_pane:
                try:
                    run_tmux_command(["kill-pane", "-t", pane_id])
                    logger.debug(f"Killed pane {pane_id} after command completion")
                except TmuxError as e:
                    logger.debug(f"Failed to kill pane {pane_id}: {e}")
                    # Don't fail the whole operation if we can't kill the pane

            return CommandResult(
                pane_id=pane_id,
                exit_code=exit_code,
                output=output,
                command=command,
                working_dir=working_dir,
                pagination=pagination_info,
            )

        finally:
            # Clean up temp directory (but keep output file for caching)
            try:
                import shutil

                shutil.rmtree(temp_dir, ignore_errors=True)
            except OSError as e:
                logger.debug(f"Failed to clean up temp dir {temp_dir}: {e}")

    except TmuxCommandError as e:
        return CommandResult(
            pane_id="",
            exit_code=-1,
            output="",
            command=command,
            working_dir=working_dir,
            error=str(e),
        )


async def tmux_send_input(pane_id: str, input_text: str) -> str:
    """Send input to a specific tmux pane."""
    try:
        run_tmux_command(["send-keys", "-t", pane_id, input_text, "Enter"])
    except TmuxError as e:
        return f"Error sending input to pane {pane_id}: {e!s}"
    else:
        return f"Input sent to pane {pane_id}: {input_text}"


async def tmux_list_sessions() -> str:
    """List all tmux sessions."""
    try:
        output = run_tmux_command(
            [
                "list-sessions",
                "-F",
                "#{session_name}: #{session_windows} windows, created #{session_created}",
            ]
        )
    except TmuxError as e:
        return f"Error listing sessions: {e!s}"
    else:
        return output if output else "No tmux sessions found"


async def tmux_list_panes(session_name: str | None = None) -> str:
    """List panes in a tmux session."""
    try:
        # Determine which session to use
        if session_name:
            target_session = session_name
        else:
            # Use current session if available, otherwise default to "mcp"
            try:
                target_session = run_tmux_command(
                    ["display-message", "-p", "#{session_name}"]
                )
            except TmuxError:
                target_session = "mcp"

        output = run_tmux_command(
            [
                "list-panes",
                "-t",
                target_session,
                "-F",
                "#{pane_id}: #{pane_title} (#{pane_width}x#{pane_height})",
            ]
        )
    except TmuxError as e:
        return f"Error listing panes: {e!s}"
    else:
        return output if output else f"No panes found in session '{target_session}'"


async def tmux_kill_pane(pane_id: str) -> str:
    """Kill a specific tmux pane."""
    try:
        run_tmux_command(["kill-pane", "-t", pane_id])
    except TmuxError as e:
        return f"Error killing pane {pane_id}: {e!s}"
    else:
        return f"Pane {pane_id} killed successfully"


async def tmux_capture_pane(pane_id: str, start_line: int | None = None) -> str:
    """Capture output from a tmux pane."""
    try:
        args = ["capture-pane", "-t", pane_id, "-p"]
        if start_line is not None:
            args.extend(["-S", str(start_line)])

        output = run_tmux_command(args)
    except TmuxError as e:
        return f"Error capturing pane {pane_id}: {e!s}"
    else:
        return output if output else f"No output captured from pane {pane_id}"


async def tmux_get_command_output(
    pane_id: str, cursor: str | None = None
) -> CommandResult:
    """Get paginated output from a previously executed command."""
    if pane_id not in _output_cache:
        return CommandResult(
            pane_id=pane_id,
            exit_code=-1,
            output="",
            command="",
            error=f"No cached output found for pane {pane_id}. Output may have been cleaned up or pane never existed.",
        )

    output_file = _output_cache[pane_id]
    if not output_file.exists():
        # Clean up stale cache entry
        _output_cache.pop(pane_id, None)
        if pane_id in _cache_cleanup_tasks:
            _cache_cleanup_tasks[pane_id].cancel()
            _cache_cleanup_tasks.pop(pane_id, None)

        return CommandResult(
            pane_id=pane_id,
            exit_code=-1,
            output="",
            command="",
            error=f"Cached output file for pane {pane_id} no longer exists",
        )

    try:
        output, pagination_info = await _read_command_output(output_file, cursor)
        return CommandResult(
            pane_id=pane_id,
            exit_code=0,
            output=output,
            command="",
            pagination=pagination_info,
        )
    except (OSError, ValueError) as e:
        return CommandResult(
            pane_id=pane_id,
            exit_code=-1,
            output="",
            command="",
            error=f"Failed to read cached output for pane {pane_id}: {e}",
        )


# Create the MCP server
server: Server[Any] = Server("tmux-mcp")


@server.list_tools()  # type: ignore[misc,no-untyped-call]
async def handle_list_tools() -> list[Tool]:
    """List available tmux tools."""
    return [
        Tool(
            name="tmux_run_command",
            description="Run a command in a new tmux window and return output when complete. Uses the current tmux session by default. Automatically closes the pane after completion unless keep_pane is True.",
            inputSchema={
                "type": "object",
                "properties": {
                    "command": {"type": "string", "description": "The command to run"},
                    "working_dir": {
                        "type": "string",
                        "description": "Working directory for the command (optional)",
                    },
                    "session_name": {
                        "type": "string",
                        "description": "Target tmux session name (defaults to current session or 'mcp')",
                    },
                    "timeout": {
                        "type": "integer",
                        "description": "Timeout in seconds (defaults to 300 seconds / 5 minutes)",
                        "default": 300,
                        "minimum": 1,
                    },
                    "keep_pane": {
                        "type": "boolean",
                        "description": "Keep the pane open after command completion for inspection (defaults to False)",
                        "default": False,
                    },
                },
                "required": ["command"],
            },
        ),
        Tool(
            name="tmux_send_input",
            description="Send input to a running command in a tmux pane",
            inputSchema={
                "type": "object",
                "properties": {
                    "pane_id": {
                        "type": "string",
                        "description": "The tmux pane ID (e.g., %0, %1)",
                    },
                    "input_text": {
                        "type": "string",
                        "description": "The input text to send",
                    },
                },
                "required": ["pane_id", "input_text"],
            },
        ),
        Tool(
            name="tmux_list_sessions",
            description="List all tmux sessions",
            inputSchema={"type": "object", "properties": {}},
        ),
        Tool(
            name="tmux_list_panes",
            description="List panes in the current tmux session (or specify a different session)",
            inputSchema={
                "type": "object",
                "properties": {
                    "session_name": {
                        "type": "string",
                        "description": "The session name (defaults to current session or 'mcp')",
                    }
                },
            },
        ),
        Tool(
            name="tmux_kill_pane",
            description="Kill a specific tmux pane",
            inputSchema={
                "type": "object",
                "properties": {
                    "pane_id": {
                        "type": "string",
                        "description": "The tmux pane ID to kill",
                    }
                },
                "required": ["pane_id"],
            },
        ),
        Tool(
            name="tmux_capture_pane",
            description="Capture output from a tmux pane",
            inputSchema={
                "type": "object",
                "properties": {
                    "pane_id": {
                        "type": "string",
                        "description": "The tmux pane ID to capture",
                    },
                    "start_line": {
                        "type": "integer",
                        "description": "Starting line number (optional)",
                    },
                },
                "required": ["pane_id"],
            },
        ),
        Tool(
            name="tmux_get_command_output",
            description="Get paginated output from a previously executed command. Use the cursor from pagination info to get subsequent pages.",
            inputSchema={
                "type": "object",
                "properties": {
                    "pane_id": {
                        "type": "string",
                        "description": "The tmux pane ID from a completed command",
                    },
                    "cursor": {
                        "type": "string",
                        "description": "Pagination cursor from previous response (format: 'start_line:max_lines'). Use '0:100' to get first 100 lines, '100:100' for next 100, etc.",
                    },
                },
                "required": ["pane_id"],
            },
        ),
    ]


async def _handle_tool_call(  # noqa: PLR0911
    name: str, arguments: dict[str, Any]
) -> CommandResult | SimpleResult:
    """Handle individual tool calls and return result."""
    match name:
        case "tmux_run_command":
            command = arguments["command"]
            working_dir = arguments.get("working_dir")
            session_name = arguments.get("session_name")
            timeout_seconds = arguments.get("timeout", 300)
            keep_pane = arguments.get("keep_pane", False)
            return await tmux_run_command(
                command, working_dir, session_name, timeout_seconds, keep_pane
            )
        case "tmux_send_input":
            pane_id = arguments["pane_id"]
            input_text = arguments["input_text"]
            message = await tmux_send_input(pane_id, input_text)
            return SimpleResult(output=message)
        case "tmux_list_sessions":
            sessions = await tmux_list_sessions()
            return SimpleResult(output=sessions)
        case "tmux_list_panes":
            session_name = arguments.get("session_name")
            panes = await tmux_list_panes(session_name)
            return SimpleResult(output=panes)
        case "tmux_kill_pane":
            pane_id = arguments["pane_id"]
            message = await tmux_kill_pane(pane_id)
            return SimpleResult(output=message)
        case "tmux_capture_pane":
            pane_id = arguments["pane_id"]
            start_line = arguments.get("start_line")
            output = await tmux_capture_pane(pane_id, start_line)
            return SimpleResult(output=output)
        case "tmux_get_command_output":
            pane_id = arguments["pane_id"]
            cursor = arguments.get("cursor")
            return await tmux_get_command_output(pane_id, cursor)
        case _:
            msg = f"Unknown tool: {name}"
            raise ValueError(msg)


@server.call_tool()  # type: ignore[misc,no-untyped-call]
async def handle_call_tool(name: str, arguments: dict[str, Any]) -> list[TextContent]:
    """Handle tool calls."""
    try:
        result = await _handle_tool_call(name, arguments)
        content_blocks = []

        if isinstance(result, CommandResult):
            # Main output block
            content_blocks.append(TextContent(type="text", text=result.output))

            # Metadata block
            metadata_parts = []
            metadata_parts.append(f"Exit code: {result.exit_code}")
            metadata_parts.append(f"Pane: {result.pane_id}")
            if result.error:
                metadata_parts.append(f"Error: {result.error}")
            if result.pagination:
                pag = result.pagination
                metadata_parts.append(
                    f"Lines: {pag.displayed_lines}/{pag.total_lines} (from line {pag.start_line + 1})"
                )
                if pag.next_cursor:
                    metadata_parts.append(f"Next cursor: {pag.next_cursor}")

            metadata_text = "Metadata: " + " | ".join(metadata_parts)
            content_blocks.append(TextContent(type="text", text=metadata_text))

        elif isinstance(result, SimpleResult):
            # Simple result - just return the output
            content_blocks.append(TextContent(type="text", text=result.output))

        return (
            content_blocks
            if content_blocks
            else [TextContent(type="text", text="No output")]
        )
    except TmuxError as e:
        logger.exception("Error in tool %s", name)
        error_message = f"Error: {e!s}"
        return [TextContent(type="text", text=error_message)]


async def main() -> None:
    """Main entry point for the tmux MCP server."""

    async with stdio_server() as (read_stream, write_stream):
        await server.run(
            read_stream, write_stream, server.create_initialization_options()
        )


def cli() -> None:
    """CLI entry point for setuptools."""
    import anyio

    anyio.run(main)


if __name__ == "__main__":
    cli()
