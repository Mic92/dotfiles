"""Tests for tmux_send_input functionality."""

import pytest
from tmux_mcp.commands import tmux_capture_pane, tmux_run_command, tmux_send_input


@pytest.mark.asyncio
async def test_tmux_send_input_and_capture(tmux_server: str) -> None:
    """Test sending input to a pane and capturing output."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Start a long-running command that waits for input, keep pane for interaction
    result = await tmux_run_command("cat", timeout_seconds=2, keep_pane=True)

    # The cat command should timeout since it waits for input
    assert result.exit_code == -1
    assert "timed out" in result.output
    pane_id = result.pane_id

    # Send input to the pane
    send_result = await tmux_send_input(pane_id, "Hello from input")
    assert send_result.exit_code == 0
    assert send_result.error is None

    # Capture the pane output
    capture_result = await tmux_capture_pane(pane_id)
    assert capture_result.exit_code == 0
    assert capture_result.error is None
    # The output should contain our input (cat echoes it back)
    assert "Hello from input" in capture_result.output


@pytest.mark.asyncio
async def test_send_input_with_expect_patterns(tmux_server: str) -> None:
    """Test send_input with expect_before and expect_after patterns."""
    _ = tmux_server  # Use the fixture to set up test environment

    # Start an interactive Python session
    result = await tmux_run_command("python3", timeout_seconds=2, keep_pane=True)
    pane_id = result.pane_id

    # Send input waiting for the Python prompt before and after
    send_result = await tmux_send_input(
        pane_id,
        "print('Hello from Python')",
        expect_before=">>>",  # Wait for Python prompt
        expect_after=">>>",  # Wait for prompt after execution
        timeout_seconds=5,
    )

    assert send_result.exit_code == 0
    assert "Hello from Python" in send_result.output
    assert send_result.error is None
