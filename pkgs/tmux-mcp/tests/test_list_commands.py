"""Tests for tmux list commands (list_sessions, list_panes)."""

import pytest
from tmux_mcp.commands import tmux_list_panes, tmux_list_sessions, tmux_run_command


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_list_sessions(tmux_server: str) -> None:
    """Test listing tmux sessions."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Ensure we have at least one session by running a command first
    await tmux_run_command("echo 'setup session'", timeout_seconds=5)

    result = await tmux_list_sessions()
    assert "test-session" in result or "mcp" in result or "windows" in result


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_list_panes(tmux_server: str) -> None:
    """Test listing panes in a session."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Create a pane first
    await tmux_run_command("echo 'create pane'", timeout_seconds=5)

    result = await tmux_list_panes()
    assert "%" in result  # Should contain pane IDs
