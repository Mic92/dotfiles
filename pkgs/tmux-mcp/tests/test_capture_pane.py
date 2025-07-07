"""Tests for tmux_capture_pane functionality."""

import pytest
from tmux_mcp.commands import tmux_capture_pane, tmux_run_command


@pytest.mark.asyncio
async def test_tmux_capture_pane_with_start_line(tmux_server: str) -> None:
    """Test capturing pane output with start line."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Create a pane with multiple lines of output
    result = await tmux_run_command(
        'for i in {1..10}; do echo "Line $i"; done', timeout_seconds=5, keep_pane=True
    )
    pane_id = result.pane_id

    # Capture all output
    capture_all = await tmux_capture_pane(pane_id)
    assert capture_all.exit_code == 0
    assert "Line 1" in capture_all.output
    assert "Line 10" in capture_all.output

    # Capture with start line
    capture_partial = await tmux_capture_pane(pane_id, start_line=5)
    assert capture_partial.exit_code == 0
    assert "Line 1\n" not in capture_partial.output  # Should not include early lines
    assert (
        "Line 5" not in capture_partial.output
    )  # Start line in tmux is 0-based, so line 5 means skip first 5 lines
    assert "Line 6" in capture_partial.output  # Should start from line 6


@pytest.mark.asyncio
async def test_capture_pane_with_expect_pattern(tmux_server: str) -> None:
    """Test capture_pane with expect pattern."""
    _ = tmux_server  # Use the fixture to set up test environment

    # Start a command that outputs gradually
    result = await tmux_run_command(
        'echo "Starting..." && sleep 1 && echo "Process ready" && sleep 60',
        timeout_seconds=2,
        keep_pane=True,
    )
    pane_id = result.pane_id

    # Capture with pattern - wait for "Process ready"
    capture_result = await tmux_capture_pane(
        pane_id, expect_pattern="Process ready", timeout_seconds=5
    )

    assert capture_result.exit_code == 0
    assert "Process ready" in capture_result.output
    assert capture_result.error is None
