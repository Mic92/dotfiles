"""Run ripgrep on cached command output functionality."""

import asyncio
import logging

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


async def tmux_ripgrep_command_output(
    pane_id: str, pattern: str, flags: str | None = None, cursor: str | None = None
) -> CommandResult:
    """Run ripgrep on cached output from a previously executed command."""
    if pane_id not in output_cache:
        return CommandResult(
            pane_id=pane_id,
            exit_code=-1,
            output="",
            command=f"rg {pattern}",
            error=f"No cached output found for pane {pane_id}. Output may have been cleaned up or pane never existed.",
        )

    output_file = output_cache[pane_id]
    if not output_file.exists():
        # Clean up stale cache entry
        output_cache.pop(pane_id, None)
        if output_file in file_cleanup_tasks:
            file_cleanup_tasks[output_file].cancel()
            file_cleanup_tasks.pop(output_file, None)

        return CommandResult(
            pane_id=pane_id,
            exit_code=-1,
            output="",
            command=f"rg {pattern}",
            error=f"Cached output file for pane {pane_id} no longer exists",
        )

    try:
        # Build ripgrep command
        rg_args = ["rg"]
        if flags:
            # Split flags on spaces and add them
            rg_args.extend(flags.split())
        rg_args.extend([pattern, str(output_file)])

        # Run ripgrep on the cached output file
        proc = await asyncio.create_subprocess_exec(
            *rg_args,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )

        stdout, stderr = await proc.communicate()
        output_text = stdout.decode()
        error_text = stderr.decode()

        # Ripgrep exit codes: 0 = matches found, 1 = no matches, 2+ = error
        exit_code = proc.returncode if proc.returncode is not None else -1
        if exit_code == 1:
            output_text = f"No matches found for pattern: {pattern}"
        elif exit_code > 1:
            output_text = f"Ripgrep error: {error_text}"
        # Apply pagination to ripgrep results only if cursor is provided
        elif cursor:
            lines = output_text.splitlines(keepends=True)
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

            output_text = "".join(displayed_lines)

            pagination_info = PaginationInfo(
                total_lines=total_lines,
                displayed_lines=len(displayed_lines),
                start_line=start_line,
                next_cursor=next_cursor,
            )

            return CommandResult(
                pane_id=pane_id,
                exit_code=exit_code,
                output=output_text,
                command=f"rg {' '.join(flags.split()) if flags else ''} {pattern}".strip(),
                pagination=pagination_info,
            )

        return CommandResult(
            pane_id=pane_id,
            exit_code=exit_code,
            output=output_text,
            command=f"rg {' '.join(flags.split()) if flags else ''} {pattern}".strip(),
        )
    except (OSError, ValueError) as e:
        return CommandResult(
            pane_id=pane_id,
            exit_code=-1,
            output="",
            command=f"rg {pattern}",
            error=f"Failed to run ripgrep on cached output for pane {pane_id}: {e}",
        )


# Tool definition
ripgrep_command_output_tool = Tool(
    name="ripgrep_command_output",
    description="Run ripgrep (rg) on cached output from a previously executed command. Searches through the full output regardless of pagination.",
    inputSchema={
        "type": "object",
        "properties": {
            "pane_id": {
                "type": "string",
                "description": "The tmux pane ID from a completed command",
            },
            "pattern": {
                "type": "string",
                "description": "The regex pattern to search for",
            },
            "flags": {
                "type": "string",
                "description": "Optional ripgrep flags (e.g., '-i' for case insensitive, '-n' for line numbers, '-C 3' for context)",
            },
            "cursor": {
                "type": "string",
                "description": "Pagination cursor from previous response (format: 'start_line:max_lines'). Use '0:100' to get first 100 lines, '100:100' for next 100, etc.",
            },
        },
        "required": ["pane_id", "pattern"],
    },
)
