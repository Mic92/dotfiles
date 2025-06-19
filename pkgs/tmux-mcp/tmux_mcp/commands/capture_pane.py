"""Capture tmux pane output functionality."""

import logging

from mcp.types import Tool

from .base import CommandResult, TmuxError, run_tmux_command, wait_for_pattern

logger = logging.getLogger("tmux-mcp")


async def tmux_capture_pane(
    pane_id: str,
    start_line: int | None = None,
    expect_pattern: str | None = None,
    timeout_seconds: float = 30,
) -> CommandResult:
    """Capture output from a tmux pane.

    If expect_pattern is provided, wait for the pattern to appear before capturing.
    Always returns a CommandResult with the captured output.
    """
    # TODO: Consider if we need both expect_before and expect_after patterns here too
    try:
        # Wait for pattern if specified
        if expect_pattern:
            logger.debug(
                f"Waiting for pattern {expect_pattern!r} before capturing with timeout: {timeout_seconds}s"
            )
            pattern_found, matched_text, current_output = await wait_for_pattern(
                pane_id, expect_pattern, timeout_seconds
            )

            if not pattern_found:
                # Pattern not found - return error with current output
                return CommandResult(
                    pane_id=pane_id,
                    exit_code=-1,
                    output=current_output,
                    command="capture_pane",
                    error=f"Pattern not found: {matched_text}",
                )

            # Pattern found - capture the output
            args = ["capture-pane", "-t", pane_id, "-p"]
            if start_line is not None:
                args.extend(["-S", str(start_line)])

            output = run_tmux_command(args)
            return CommandResult(
                pane_id=pane_id,
                exit_code=0,
                output=output if output else "",
                command="capture_pane",
            )

        # No expect pattern - capture immediately
        args = ["capture-pane", "-t", pane_id, "-p"]
        if start_line is not None:
            args.extend(["-S", str(start_line)])

        output = run_tmux_command(args)

        return CommandResult(
            pane_id=pane_id,
            exit_code=0,
            output=output if output else "",
            command="capture_pane",
        )
    except TmuxError as e:
        return CommandResult(
            pane_id=pane_id,
            exit_code=-1,
            output="",
            command="capture_pane",
            error=f"Error capturing pane {pane_id}: {e!s}",
        )


# Tool definition
capture_pane_tool = Tool(
    name="capture_pane",
    description="Capture output from a tmux pane. If expect_pattern is provided, waits for the pattern before capturing.",
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
            "expect_pattern": {
                "type": "string",
                "description": "Regex pattern to wait for before capturing output",
            },
            "timeout": {
                "type": "number",
                "description": "Timeout in seconds for expect_pattern (defaults to 30 seconds)",
                "default": 30,
                "minimum": 0.1,
            },
        },
        "required": ["pane_id"],
    },
)
