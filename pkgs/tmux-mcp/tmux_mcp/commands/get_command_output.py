"""Get paginated command output functionality."""

import logging
from pathlib import Path

import anyio
from mcp.types import Tool

from .base import CommandResult, PaginationInfo, file_cleanup_tasks, output_cache

logger = logging.getLogger("tmux-mcp")


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


async def tmux_get_command_output(
    pane_id: str, cursor: str | None = None
) -> CommandResult:
    """Get paginated output from a previously executed command."""
    logger.debug(f"Getting command output for pane {pane_id}, cursor={cursor}")

    if pane_id not in output_cache:
        logger.debug(
            f"Pane {pane_id} not in cache. Cache keys: {list(output_cache.keys())}"
        )
        return CommandResult(
            pane_id=pane_id,
            exit_code=-1,
            output="",
            command="",
            error=f"No cached output found for pane {pane_id}. Output may have been cleaned up or pane never existed.",
        )

    output_file = output_cache[pane_id]
    logger.debug(f"Found cached file for pane {pane_id}: {output_file}")

    if not output_file.exists():
        logger.debug(f"Cached file {output_file} does not exist!")
        # Clean up stale cache entry
        output_cache.pop(pane_id, None)
        if output_file in file_cleanup_tasks:
            file_cleanup_tasks[output_file].cancel()
            file_cleanup_tasks.pop(output_file, None)

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


# Tool definition
get_command_output_tool = Tool(
    name="get_command_output",
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
)
