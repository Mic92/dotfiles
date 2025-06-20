"""Base classes and utilities for tmux MCP commands."""

import asyncio
import logging
import re
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path

logger = logging.getLogger("tmux-mcp")


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


@dataclass
class TmuxEnvironment:
    """Tmux environment setup for command execution."""

    pane_id: str
    completion_fifo: Path
    output_file: Path
    temp_dir_context: tempfile.TemporaryDirectory[str]


# Global cache for command outputs
# Maps pane_id -> output_file_path
output_cache: dict[str, Path] = {}
# File cleanup registry - maps file path to cleanup task
file_cleanup_tasks: dict[Path, asyncio.Task[None]] = {}
# Window cleanup registry
window_cleanup_tasks: dict[str, asyncio.Task[None]] = {}


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


def get_target_session(session_name: str | None) -> str:
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


async def wait_for_pattern(
    pane_id: str, pattern: str, timeout_seconds: float, output_file: Path | None = None
) -> tuple[bool, str, str]:
    """Wait for a regex pattern to appear in command output.

    Returns (found, matched_text, current_output) where:
    - found: True if pattern was found
    - matched_text: The matched text or error message
    - current_output: The current output (especially useful on timeout)
    """
    try:
        regex = re.compile(pattern)
    except re.error as e:
        logger.exception(f"Invalid regex pattern {pattern!r}")
        return False, f"Invalid regex pattern: {e}", ""

    start_time = asyncio.get_event_loop().time()
    poll_interval = 0.1  # Start with 100ms

    try:
        while True:
            # Check if timeout exceeded
            elapsed = asyncio.get_event_loop().time() - start_time
            if elapsed >= timeout_seconds:
                # Get final output state
                current_output = ""
                if output_file and output_file.exists():
                    try:
                        current_output = output_file.read_text()
                    except Exception:
                        logger.exception("Failed to read output file on timeout")

                if not current_output:
                    # Fall back to tmux capture
                    try:
                        current_output = run_tmux_command(
                            ["capture-pane", "-t", pane_id, "-p"]
                        )
                    except Exception:
                        logger.exception("Failed to capture pane on timeout")

                return (
                    False,
                    f"Pattern '{pattern}' not found within {timeout_seconds}s timeout",
                    current_output,
                )

            # Get current output
            try:
                if output_file and output_file.exists():
                    # Prefer reading from output file for clean output
                    current_output = output_file.read_text()
                else:
                    # Fall back to tmux capture (e.g., for send_input, capture_pane)
                    current_output = run_tmux_command(
                        ["capture-pane", "-t", pane_id, "-p"]
                    )
            except (TmuxError, OSError) as e:
                logger.debug(f"Failed to get output: {e}")
                await asyncio.sleep(poll_interval)
                # Exponential backoff with max of 1 second
                poll_interval = min(poll_interval * 1.5, 1.0)
                continue

            # Check for pattern
            match = regex.search(current_output)
            if match:
                return True, match.group(0), current_output

            # Wait before next check
            await asyncio.sleep(poll_interval)
            # Exponential backoff with max of 1 second
            poll_interval = min(poll_interval * 1.5, 1.0)
    except asyncio.CancelledError:
        logger.debug("Pattern wait cancelled")
        raise
    except Exception as e:
        logger.exception("Error waiting for pattern")
        return False, f"Error waiting for pattern: {e}", ""


async def cleanup_output_file(file_path: Path, delay_seconds: int = 300) -> None:
    """Clean up an output file after a delay."""
    try:
        await asyncio.sleep(delay_seconds)
        if file_path.exists():
            file_path.unlink()
            logger.debug(f"Cleaned up output file: {file_path}")
    except asyncio.CancelledError:
        # If cancelled, still try to clean up
        if file_path.exists():
            file_path.unlink()
        raise
    except Exception:
        logger.exception(f"Error cleaning up output file {file_path}")
    finally:
        # Remove from cleanup registry
        file_cleanup_tasks.pop(file_path, None)
