"""List tmux panes functionality."""

from mcp.types import Tool

from .base import TmuxError, run_tmux_command


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


# Tool definition
list_panes_tool = Tool(
    name="list_panes",
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
)
