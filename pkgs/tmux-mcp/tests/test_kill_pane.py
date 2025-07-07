"""Tests for tmux_kill_pane functionality."""

import pytest
from tmux_mcp.commands import tmux_kill_pane, tmux_run_command


@pytest.mark.asyncio
async def test_tmux_kill_pane(tmux_server: str) -> None:
    """Test killing a tmux pane."""
    _ = tmux_server  # Use the fixture to set up test environment

    # Create a pane
    result = await tmux_run_command("sleep 60", timeout_seconds=2, keep_pane=True)
    pane_id = result.pane_id

    # Verify the command timed out but pane was created
    assert result.exit_code == -1  # Timeout
    assert result.error is not None
    assert "timeout" in result.error
    assert pane_id.startswith("%")

    # Kill the pane
    kill_result = await tmux_kill_pane(pane_id)
    assert "killed" in kill_result.lower() or "successfully" in kill_result.lower()
