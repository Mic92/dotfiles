"""Run command in tmux pane functionality."""

import asyncio
import contextlib
import logging
import os
import random
import shlex
import tempfile
from dataclasses import dataclass
from pathlib import Path

import anyio
from mcp.types import Tool

from .base import (
    CommandResult,
    PaginationInfo,
    TmuxCommandError,
    TmuxEnvironment,
    TmuxError,
    cleanup_output_file,
    file_cleanup_tasks,
    get_target_session,
    output_cache,
    run_tmux_command,
    wait_for_pattern,
    window_cleanup_tasks,
)

logger = logging.getLogger("tmux-mcp")


@dataclass
class CommandContext:
    """Context for command execution."""

    pane_id: str
    command: str
    working_dir: str | None
    output_file: Path
    keep_pane: bool
    timeout_seconds: int


def _build_shell_command(
    command: str,
    working_dir: str | None,
    completion_fifo: Path,
    output_file: Path,
    keep_pane: bool = False,
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
    # 5. If keep_pane is true, sleep forever to keep the pane alive
    script_content = f"""
# Change to working directory if specified
{f"cd {quoted_working_dir} &&" if working_dir else ""}

# Run command with tee to live stream output and capture to file
({command}) 2>&1 | tee {quoted_output_file}
exit_code=${{PIPESTATUS[0]}}

# Signal completion by writing to FIFO
echo "done:$exit_code" > {quoted_completion_fifo}

# If keep_pane is true, sleep forever to keep the pane alive (portable)
{"while true; do sleep 86400; done" if keep_pane else ""}
"""

    # Return the bash command that executes our script
    return f"bash -c {shlex.quote(script_content)}"


def _create_tmux_window(
    target_session: str,
    working_dir: str | None,
    shell_command: str,
    window_name: str | None = None,
) -> str:
    """Create a new tmux window and return its unique name."""
    # Use provided window name or generate a unique one
    if not window_name:
        window_name = f"claude-{random.randint(100, 9999)}"  # noqa: S311

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

    # Create window and run the command directly
    window_args.append(shell_command)
    run_tmux_command(window_args)

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


def _cache_output_file(pane_id: str, output_file: Path) -> None:
    """Cache an output file for later pagination."""
    logger.debug(f"Caching output file for pane {pane_id}: {output_file}")

    # If this pane already has a cached file, cancel its cleanup
    if pane_id in output_cache:
        old_file = output_cache[pane_id]
        if old_file in file_cleanup_tasks:
            logger.debug(f"Cancelling cleanup task for old file {old_file}")
            with contextlib.suppress(RuntimeError):
                file_cleanup_tasks[old_file].cancel()
            file_cleanup_tasks.pop(old_file, None)

    # Store the output file path
    output_cache[pane_id] = output_file

    # Schedule cleanup for this specific file after 5 minutes
    try:
        loop = asyncio.get_running_loop()
        if not loop.is_closed():
            cleanup_task = asyncio.create_task(cleanup_output_file(output_file))
            file_cleanup_tasks[output_file] = cleanup_task
            logger.debug(f"Scheduled file cleanup for {output_file}")
    except RuntimeError:
        # No running event loop - skip cleanup task scheduling
        logger.debug(f"No running event loop - skipping cleanup task for {output_file}")


async def _cleanup_tmux_window(pane_id: str, delay_seconds: int = 60) -> None:
    """Clean up tmux window after a delay."""
    try:
        await asyncio.sleep(delay_seconds)
        try:
            # Kill the pane which will close the window if it's the only pane
            run_tmux_command(["kill-pane", "-t", pane_id])
            logger.debug(
                f"Cleaned up tmux window for pane {pane_id} after {delay_seconds}s delay"
            )
        except TmuxError as e:
            logger.debug(f"Failed to clean up window for pane {pane_id}: {e}")
        finally:
            window_cleanup_tasks.pop(pane_id, None)
    except asyncio.CancelledError:
        # Task was cancelled, still clean up tracking
        window_cleanup_tasks.pop(pane_id, None)
        raise


def _schedule_window_cleanup(pane_id: str, delay_seconds: int = 60) -> None:
    """Schedule window cleanup after a delay."""
    # Cancel any existing cleanup task for this pane (safely)
    if pane_id in window_cleanup_tasks:
        with contextlib.suppress(RuntimeError):
            # Event loop might be closed - ignore cancellation errors
            window_cleanup_tasks[pane_id].cancel()
        window_cleanup_tasks.pop(pane_id, None)

    # Schedule cleanup (only if event loop is running)
    try:
        loop = asyncio.get_running_loop()
        if not loop.is_closed():
            cleanup_task = asyncio.create_task(
                _cleanup_tmux_window(pane_id, delay_seconds)
            )
            window_cleanup_tasks[pane_id] = cleanup_task
    except RuntimeError:
        # No running event loop - skip cleanup task scheduling
        pass


async def _setup_tmux_environment(
    command: str,
    working_dir: str | None,
    session_name: str | None,
    window_name: str | None,
    keep_pane: bool = False,
) -> TmuxEnvironment:
    """Set up tmux environment for command execution.

    Returns: TmuxEnvironment with all necessary resources
    """
    target_session = get_target_session(session_name)
    logger.debug(f"Target session: {target_session}")

    # Create temporary directory for FIFO
    temp_dir_context = tempfile.TemporaryDirectory()
    temp_dir_path = Path(temp_dir_context.name)
    completion_fifo = temp_dir_path / "completion.fifo"

    # Create output file that won't be cleaned up with temp_dir
    with tempfile.NamedTemporaryFile(
        mode="w+b", suffix=".txt", prefix="tmux_output_", delete=False
    ) as output_file_context:
        output_file = Path(output_file_context.name)

    logger.debug(
        f"Created temp dir: {temp_dir_context.name}, FIFO: {completion_fifo}, output: {output_file}"
    )

    # Create FIFO for completion signaling
    os.mkfifo(str(completion_fifo))
    logger.debug(f"Created FIFO: {completion_fifo}")

    # Build and execute the command
    shell_command = _build_shell_command(
        command, working_dir, completion_fifo, output_file, keep_pane
    )
    logger.debug(f"Built shell command: {shell_command}")

    window_name_result = _create_tmux_window(
        target_session, working_dir, shell_command, window_name
    )
    logger.debug(f"Created window: {window_name_result}")

    pane_id = _find_pane_id(target_session, window_name_result)
    logger.debug(f"Found pane ID: {pane_id}")

    return TmuxEnvironment(
        pane_id=pane_id,
        completion_fifo=completion_fifo,
        output_file=output_file,
        temp_dir_context=temp_dir_context,
    )


async def _handle_expect_pattern(
    ctx: CommandContext,
    expect_pattern: str,
) -> CommandResult | None:
    """Handle expect pattern logic for tmux_run_command.

    Returns CommandResult if pattern handling is complete, None to continue normal flow.
    """
    logger.debug(
        f"Starting to wait for pattern {expect_pattern!r} with timeout: {ctx.timeout_seconds}s"
    )
    pattern_found, matched_text, current_output = await wait_for_pattern(
        ctx.pane_id, expect_pattern, ctx.timeout_seconds, ctx.output_file
    )

    if not pattern_found:
        # Pattern not found - treat as error
        if not ctx.keep_pane:
            _schedule_window_cleanup(ctx.pane_id, delay_seconds=60)
            logger.debug(
                f"Scheduled window cleanup for pane {ctx.pane_id} in 60s after pattern timeout"
            )

        return CommandResult(
            pane_id=ctx.pane_id,
            exit_code=-1,
            output=current_output,
            command=ctx.command,
            working_dir=ctx.working_dir,
            error=f"Pattern not found: {matched_text}",
        )

    # Pattern found - command continues running if keep_pane is True
    logger.debug(f"Pattern found: {matched_text!r}")
    return CommandResult(
        pane_id=ctx.pane_id,
        exit_code=0,
        output=current_output,
        command=ctx.command,
        working_dir=ctx.working_dir,
    )


async def _handle_command_completion(
    ctx: CommandContext,
    completion_fifo: Path,
) -> CommandResult:
    """Handle normal command completion (non-expect pattern mode)."""
    logger.debug(
        f"Starting to wait for completion with timeout: {ctx.timeout_seconds}s"
    )
    completion_task = asyncio.create_task(_wait_for_completion(completion_fifo))

    try:
        success, exit_code = await asyncio.wait_for(
            completion_task, timeout=ctx.timeout_seconds
        )
        logger.debug(
            f"Completion wait finished: success={success}, exit_code={exit_code}"
        )
    except TimeoutError:
        logger.debug(f"Command timed out after {ctx.timeout_seconds} seconds")
        # Cancel the completion task to clean up subprocess
        completion_task.cancel()
        with contextlib.suppress(asyncio.CancelledError):
            await completion_task

        # Schedule window cleanup with 60s delay since it timed out
        if not ctx.keep_pane:
            _schedule_window_cleanup(ctx.pane_id, delay_seconds=60)
            logger.debug(
                f"Scheduled window cleanup for pane {ctx.pane_id} in 60s after timeout"
            )

        return CommandResult(
            pane_id=ctx.pane_id,
            exit_code=-1,
            output=f"Command timed out after {ctx.timeout_seconds} seconds",
            command=ctx.command,
            working_dir=ctx.working_dir,
            error="timeout",
        )

    if not success:
        return CommandResult(
            pane_id=ctx.pane_id,
            exit_code=-1,
            output="Failed to receive completion signal",
            command=ctx.command,
            working_dir=ctx.working_dir,
            error="completion_signal_failed",
        )

    # Read the output for initial response (first 100 lines by default)
    output, pagination_info = await _read_command_output(ctx.output_file, "0:100")

    # Cache the output file for pagination
    _cache_output_file(ctx.pane_id, ctx.output_file)

    # Schedule window cleanup with 60s delay (unless keep_pane is True)
    if not ctx.keep_pane:
        _schedule_window_cleanup(ctx.pane_id, delay_seconds=60)
        logger.debug(f"Scheduled window cleanup for pane {ctx.pane_id} in 60s")

    return CommandResult(
        pane_id=ctx.pane_id,
        exit_code=exit_code,
        output=output,
        command=ctx.command,
        working_dir=ctx.working_dir,
        pagination=pagination_info,
    )


async def tmux_run_command(  # noqa: PLR0913
    command: str,
    working_dir: str | None = None,
    session_name: str | None = None,
    timeout_seconds: int = 300,
    keep_pane: bool = False,
    window_name: str | None = None,
    expect_pattern: str | None = None,
) -> CommandResult:
    """Run a command in a new tmux pane and wait for completion using FIFO synchronization.

    If expect_pattern is provided, wait for the pattern to appear in output instead of
    waiting for command completion. The timeout_seconds parameter applies to the pattern wait.
    """
    logger.debug(
        f"Starting tmux_run_command: command={command!r}, working_dir={working_dir!r}, "
        f"session_name={session_name!r}, timeout={timeout_seconds}, window_name={window_name!r}, "
        f"expect_pattern={expect_pattern!r}"
    )

    env = None
    try:
        # Set up tmux environment
        env = await _setup_tmux_environment(
            command, working_dir, session_name, window_name, keep_pane
        )

        # Create context object
        ctx = CommandContext(
            pane_id=env.pane_id,
            command=command,
            working_dir=working_dir,
            output_file=env.output_file,
            keep_pane=keep_pane,
            timeout_seconds=timeout_seconds,
        )

        # Handle expect pattern if provided
        if expect_pattern:
            result = await _handle_expect_pattern(ctx, expect_pattern)
            if result:
                return result

        # Handle normal command completion
        return await _handle_command_completion(ctx, env.completion_fifo)

    except TmuxCommandError as e:
        return CommandResult(
            pane_id="",
            exit_code=-1,
            output="",
            command=command,
            working_dir=working_dir,
            error=str(e),
        )
    finally:
        # Clean up temp directory (output file is kept for caching)
        if env:
            env.temp_dir_context.cleanup()
            logger.debug(f"Cleaned up temp directory for pane {env.pane_id}")


# Tool definition
run_command_tool = Tool(
    name="run_command",
    description="Run a command in a new tmux window and return output when complete. Uses the current tmux session by default. Automatically closes the window after 60 seconds unless keep_pane is True. If expect_pattern is provided, waits for the regex pattern to appear in output instead of waiting for command completion.",
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
                "description": "Timeout in seconds (defaults to 300 seconds / 5 minutes). When expect_pattern is provided, this is the timeout for the pattern to appear.",
                "default": 300,
                "minimum": 1,
            },
            "keep_pane": {
                "type": "boolean",
                "description": "Keep the pane open after command completion or pattern match (defaults to False)",
                "default": False,
            },
            "window_name": {
                "type": "string",
                "description": "Custom name for the tmux window (defaults to auto-generated name)",
            },
            "expect_pattern": {
                "type": "string",
                "description": "Regex pattern to wait for in the output instead of waiting for command completion. The timeout parameter specifies how long to wait for this pattern.",
            },
        },
        "required": ["command"],
    },
)
