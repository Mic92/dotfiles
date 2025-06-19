"""Tests for ripgrep functionality in tmux commands."""

import pytest
from tmux_mcp.commands import (
    tmux_ripgrep_command_output,
    tmux_run_command,
)


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_ripgrep_command_output_basic(tmux_server: str) -> None:
    """Test basic ripgrep functionality on command output."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Create output with mixed content
    result = await tmux_run_command(
        "echo 'Hello World' && echo 'Goodbye Moon' && echo 'Hello Again'",
        timeout_seconds=5,
    )

    # Search for "Hello"
    rg_result = await tmux_ripgrep_command_output(result.pane_id, "Hello")
    assert rg_result.exit_code == 0
    assert "Hello World" in rg_result.output
    assert "Hello Again" in rg_result.output
    assert "Goodbye Moon" not in rg_result.output


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_ripgrep_command_output_with_flags(
    tmux_server: str,
) -> None:
    """Test ripgrep with various flags."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Create output with patterns
    result = await tmux_run_command(
        "echo 'HELLO world' && echo 'hello WORLD' && echo 'HeLLo WoRLd'",
        timeout_seconds=5,
    )

    # Case insensitive search with line numbers
    rg_result = await tmux_ripgrep_command_output(
        result.pane_id, "hello", flags="-i -n"
    )
    assert rg_result.exit_code == 0
    # Should match all three lines
    assert "HELLO world" in rg_result.output
    assert "hello WORLD" in rg_result.output
    assert "HeLLo WoRLd" in rg_result.output


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_ripgrep_command_output_no_matches(
    tmux_server: str,
) -> None:
    """Test ripgrep with no matches."""
    _ = tmux_server  # Use the fixture to set up test environment
    result = await tmux_run_command("echo 'Hello World'", timeout_seconds=5)

    # Search for non-existent pattern
    rg_result = await tmux_ripgrep_command_output(result.pane_id, "NonExistent")
    assert rg_result.exit_code == 1  # ripgrep returns 1 for no matches
    assert "No matches found for pattern: NonExistent" in rg_result.output


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_ripgrep_command_output_invalid_pane(
    tmux_server: str,
) -> None:
    """Test ripgrep with invalid pane ID."""
    _ = tmux_server  # Use the fixture to set up test environment
    result = await tmux_ripgrep_command_output("%invalid", "test")

    assert result.exit_code == -1
    assert (
        result.error
        == "No cached output found for pane %invalid. Output may have been cleaned up or pane never existed."
    )


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_ripgrep_pagination_basic(tmux_server: str) -> None:
    """Test ripgrep with pagination."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Create many lines with pattern
    result = await tmux_run_command(
        'for i in {1..150}; do echo "Line $i contains TEST"; done', timeout_seconds=10
    )

    # Search with pagination using cursor
    rg_result = await tmux_ripgrep_command_output(
        result.pane_id, "TEST", cursor="0:100"
    )
    assert rg_result.exit_code == 0
    assert rg_result.pagination is not None
    assert rg_result.pagination.displayed_lines == 100  # Requested page size
    assert "Line 1 contains TEST" in rg_result.output
    assert "Line 100 contains TEST" in rg_result.output
    assert "Line 150 contains TEST" not in rg_result.output  # Beyond first page


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_ripgrep_pagination_next_page(tmux_server: str) -> None:
    """Test ripgrep pagination - getting next page."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Create many lines
    result = await tmux_run_command(
        'for i in {1..150}; do echo "Line $i has MATCH"; done', timeout_seconds=10
    )

    # Get first page
    rg_result1 = await tmux_ripgrep_command_output(
        result.pane_id, "MATCH", cursor="0:50"
    )
    assert rg_result1.exit_code == 0
    assert rg_result1.pagination is not None
    assert rg_result1.pagination.displayed_lines == 50

    # Get next page
    rg_result2 = await tmux_ripgrep_command_output(
        result.pane_id, "MATCH", cursor="50:50"
    )
    assert rg_result2.exit_code == 0
    assert "Line 51 has MATCH" in rg_result2.output
    assert "Line 100 has MATCH" in rg_result2.output


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_ripgrep_no_pagination(tmux_server: str) -> None:
    """Test ripgrep with small output (no pagination needed)."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Create small output
    result = await tmux_run_command(
        "echo 'Match 1' && echo 'No match' && echo 'Match 2'", timeout_seconds=5
    )

    # Search - should get all results
    rg_result = await tmux_ripgrep_command_output(result.pane_id, "Match")
    assert rg_result.exit_code == 0
    assert rg_result.pagination is None  # No pagination for small output
    assert "Match 1" in rg_result.output
    assert "Match 2" in rg_result.output
    assert "No match" not in rg_result.output
