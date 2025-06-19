"""Tests for pagination functionality in tmux commands."""

import pytest
from tmux_mcp.commands import tmux_get_command_output, tmux_run_command


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_pagination_basic(tmux_server: str) -> None:
    """Test basic pagination functionality."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Create a command with lots of output (150 lines)
    result = await tmux_run_command("seq 1 150", timeout_seconds=10)

    assert result.exit_code == 0
    assert result.pagination is not None

    # Check pagination info
    assert result.pagination.total_lines == 150
    assert result.pagination.displayed_lines == 100  # Default page size
    assert result.pagination.start_line == 0

    # Output should contain first 100 lines
    assert "1" in result.output
    assert "100" in result.output
    assert "150" not in result.output  # Beyond first page

    # Get next page
    next_page_result = await tmux_get_command_output(result.pane_id, "100:50")
    assert next_page_result.exit_code == 0
    assert next_page_result.pagination is not None
    assert next_page_result.pagination.displayed_lines == 50
    assert next_page_result.pagination.start_line == 100
    assert "101" in next_page_result.output
    assert "150" in next_page_result.output


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_pagination_custom_cursor(tmux_server: str) -> None:
    """Test pagination with custom cursor."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Create output
    result = await tmux_run_command("seq 1 50", timeout_seconds=5)
    pane_id = result.pane_id

    # Get custom page
    custom_result = await tmux_get_command_output(pane_id, "10:20")
    assert custom_result.exit_code == 0
    assert custom_result.pagination is not None
    assert custom_result.pagination.start_line == 10
    assert custom_result.pagination.displayed_lines == 20
    assert "11" in custom_result.output  # seq is 1-based, line 10 is "11"
    assert "30" in custom_result.output


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_pagination_invalid_pane(tmux_server: str) -> None:
    """Test pagination with invalid pane ID."""
    _ = tmux_server  # Use the fixture to set up test environment
    result = await tmux_get_command_output("%invalid", "0:10")

    assert result.exit_code == -1
    assert (
        result.error
        == "No cached output found for pane %invalid. Output may have been cleaned up or pane never existed."
    )


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_pagination_no_cursor(tmux_server: str) -> None:
    """Test pagination without cursor (default behavior)."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Create output
    result = await tmux_run_command("seq 1 50", timeout_seconds=5)
    pane_id = result.pane_id

    # Get without cursor - should return first 100 lines
    default_result = await tmux_get_command_output(pane_id)
    assert default_result.exit_code == 0
    assert default_result.pagination is not None
    assert default_result.pagination.start_line == 0
    assert default_result.pagination.displayed_lines == 50  # All 50 lines
    assert "1" in default_result.output
    assert "50" in default_result.output
