"""List tmux sessions functionality."""

from mcp.types import Tool

from .base import TmuxError, run_tmux_command


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


# Tool definition
list_sessions_tool = Tool(
    name="list_sessions",
    description="List all tmux sessions",
    inputSchema={"type": "object", "properties": {}},
)
