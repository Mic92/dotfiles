"""Kill tmux pane functionality."""

from mcp.types import Tool

from .base import TmuxError, run_tmux_command


async def tmux_kill_pane(pane_id: str) -> str:
    """Kill a specific tmux pane."""
    try:
        run_tmux_command(["kill-pane", "-t", pane_id])
    except TmuxError as e:
        return f"Error killing pane {pane_id}: {e!s}"
    else:
        return f"Pane {pane_id} killed successfully"


# Tool definition
kill_pane_tool = Tool(
    name="kill_pane",
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
)
