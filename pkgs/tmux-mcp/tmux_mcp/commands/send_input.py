"""Send input to tmux pane functionality."""

import logging

from mcp.types import Tool

from .base import CommandResult, TmuxError, run_tmux_command, wait_for_pattern

logger = logging.getLogger("tmux-mcp")


async def tmux_send_input(
    pane_id: str,
    input_text: str,
    expect_before: str | None = None,
    expect_after: str | None = None,
    timeout_seconds: float = 30,
) -> CommandResult:
    """Send input to a specific tmux pane.

    If expect_before is provided, wait for this pattern before sending input.
    If expect_after is provided, wait for this pattern after sending input.
    Always returns a CommandResult with the pane output.
    """
    try:
        # Wait for pattern before sending input if specified
        if expect_before:
            logger.debug(
                f"Waiting for pattern {expect_before!r} before sending input with timeout: {timeout_seconds}s"
            )
            pattern_found, matched_text, current_output = await wait_for_pattern(
                pane_id, expect_before, timeout_seconds
            )

            if not pattern_found:
                # Pattern not found - return error with current output
                return CommandResult(
                    pane_id=pane_id,
                    exit_code=-1,
                    output=current_output,
                    command=f"send_input: {input_text}",
                    error=f"Pattern not found before sending input: {matched_text}",
                )

        # Send the input
        run_tmux_command(["send-keys", "-t", pane_id, input_text, "Enter"])

        # Wait for pattern after sending input if specified
        if expect_after:
            logger.debug(
                f"Waiting for pattern {expect_after!r} after sending input with timeout: {timeout_seconds}s"
            )
            pattern_found, matched_text, current_output = await wait_for_pattern(
                pane_id, expect_after, timeout_seconds
            )

            if not pattern_found:
                # Pattern not found - return error with current output
                return CommandResult(
                    pane_id=pane_id,
                    exit_code=-1,
                    output=current_output,
                    command=f"send_input: {input_text}",
                    error=f"Pattern not found after sending input: {matched_text}",
                )

            # Pattern found - return success with output
            return CommandResult(
                pane_id=pane_id,
                exit_code=0,
                output=current_output,
                command=f"send_input: {input_text}",
            )

        # No expect_after pattern - capture current output and return success
        try:
            current_output = run_tmux_command(["capture-pane", "-t", pane_id, "-p"])
        except TmuxError:
            current_output = ""

        return CommandResult(
            pane_id=pane_id,
            exit_code=0,
            output=current_output,
            command=f"send_input: {input_text}",
        )

    except TmuxError as e:
        return CommandResult(
            pane_id=pane_id,
            exit_code=-1,
            output="",
            command=f"send_input: {input_text}",
            error=f"Error sending input to pane {pane_id}: {e!s}",
        )


# Tool definition
send_input_tool = Tool(
    name="send_input",
    description="Send input to a running command in a tmux pane. Can wait for patterns before and/or after sending input.",
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
            "expect_before": {
                "type": "string",
                "description": "Regex pattern to wait for before sending input (e.g., wait for a prompt)",
            },
            "expect_after": {
                "type": "string",
                "description": "Regex pattern to wait for after sending input (e.g., wait for command completion)",
            },
            "timeout": {
                "type": "number",
                "description": "Timeout in seconds for each expect pattern (defaults to 30 seconds)",
                "default": 30,
                "minimum": 0.1,
            },
        },
        "required": ["pane_id", "input_text"],
    },
)
