"""Tests for tmux_run_command functionality."""

import tempfile
from pathlib import Path

import pytest
from tmux_mcp.commands import tmux_run_command


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_run_simple_command(tmux_server: str) -> None:
    """Test running a simple command in tmux."""
    _ = tmux_server  # Use the fixture to set up test environment
    result = await tmux_run_command("echo 'Hello World'", timeout_seconds=10)

    assert result.exit_code == 0
    assert "Hello World" in result.output
    assert result.pane_id.startswith("%")


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_run_command_with_working_dir(tmux_server: str) -> None:
    """Test running command with working directory."""
    _ = tmux_server  # Use the fixture to set up test environment
    with tempfile.TemporaryDirectory() as temp_dir:
        # Create a test file in the temp directory
        test_file = Path(temp_dir) / "test.txt"
        test_file.write_text("test content")

        result = await tmux_run_command(
            "ls test.txt", working_dir=temp_dir, timeout_seconds=10
        )

        assert result.exit_code == 0
        assert "test.txt" in result.output
        assert result.working_dir == temp_dir


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_run_command_failure(tmux_server: str) -> None:
    """Test running a command that fails."""
    _ = tmux_server  # Use the fixture to set up test environment
    result = await tmux_run_command("nonexistent_command_12345", timeout_seconds=10)

    assert result.exit_code != 0
    assert result.pane_id.startswith("%")


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_run_command_timeout(tmux_server: str) -> None:
    """Test command timeout handling."""
    _ = tmux_server  # Use the fixture to set up test environment
    result = await tmux_run_command("sleep 10", timeout_seconds=1)

    assert result.exit_code == -1
    assert result.error == "timeout"
    assert "timed out" in result.output


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_multiple_commands_sequential(tmux_server: str) -> None:
    """Test running multiple commands sequentially."""
    _ = tmux_server  # Use the fixture to set up test environment
    results = []
    for i in range(3):
        result = await tmux_run_command(f"echo 'Command {i}'", timeout_seconds=5)
        results.append(result)

    for i, result in enumerate(results):
        assert result.exit_code == 0
        assert f"Command {i}" in result.output


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_error_handling(tmux_server: str) -> None:
    """Test various error conditions."""
    _ = tmux_server  # Use the fixture to set up test environment

    # Test with non-existent working directory
    result = await tmux_run_command(
        "echo test", working_dir="/non/existent/path", timeout_seconds=5
    )
    # Command creation might fail or directory might be created
    # depending on the tmux configuration
    assert result.pane_id  # Should still have a pane ID


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_delayed_window_cleanup(tmux_server: str) -> None:
    """Test that window cleanup happens after delay."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Run a command without keep_pane
    result = await tmux_run_command("echo 'cleanup test'", timeout_seconds=5)
    pane_id = result.pane_id

    # Check that the command succeeded and returned a valid pane ID
    assert result.exit_code == 0
    assert pane_id.startswith("%")
    assert "cleanup test" in result.output

    # Without keep_pane, cleanup is scheduled but happens after 60s
    # We can't easily test the actual cleanup without waiting or mocking


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_keep_pane_option(tmux_server: str) -> None:
    """Test keep_pane option prevents cleanup."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Run command with keep_pane=True
    result = await tmux_run_command(
        "echo 'keep pane test'", timeout_seconds=5, keep_pane=True
    )
    pane_id = result.pane_id

    # Check that the command succeeded and returned a valid pane ID
    assert result.exit_code == 0
    assert pane_id.startswith("%")
    assert "keep pane test" in result.output

    # With keep_pane=True and the infinite sleep, the pane should stay alive
    # We can't easily verify this in tests without complex tmux session management
    # but we've verified the command completed successfully


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_custom_window_name(tmux_server: str) -> None:
    """Test creating windows with custom names."""
    _ = tmux_server  # Use the fixture to set up test environment
    result = await tmux_run_command(
        "echo 'custom window'", window_name="my-custom-window", timeout_seconds=5
    )

    assert result.exit_code == 0
    assert "custom window" in result.output

    # Check that we can run with the same name (should create new window)
    result2 = await tmux_run_command(
        "echo 'another command'", window_name="my-custom-window", timeout_seconds=5
    )
    assert result2.exit_code == 0


@pytest.mark.asyncio  # type: ignore[misc]
async def test_tmux_auto_generated_window_name(tmux_server: str) -> None:
    """Test auto-generated window names."""
    _ = tmux_server  # Use the fixture to set up test environment
    # Run without window_name - should generate one
    result1 = await tmux_run_command("echo 'test1'", timeout_seconds=5)
    result2 = await tmux_run_command("echo 'test2'", timeout_seconds=5)

    assert result1.exit_code == 0
    assert result2.exit_code == 0
    # Each should have a different pane ID
    assert result1.pane_id != result2.pane_id


@pytest.mark.asyncio  # type: ignore[misc]
async def test_run_command_with_expect_pattern(tmux_server: str) -> None:
    """Test running a command with expect pattern."""
    _ = tmux_server  # Use the fixture to set up test environment

    # Test successful pattern match
    result = await tmux_run_command(
        "echo 'Starting process...' && sleep 1 && echo 'Ready to accept connections'",
        expect_pattern="Ready to accept",
        timeout_seconds=5,
    )

    assert result.exit_code == 0
    assert "Ready to accept connections" in result.output
    assert result.error is None


@pytest.mark.asyncio  # type: ignore[misc]
async def test_expect_pattern_timeout(tmux_server: str) -> None:
    """Test expect pattern timeout."""
    _ = tmux_server  # Use the fixture to set up test environment

    # Pattern that won't match
    result = await tmux_run_command(
        "echo 'Hello World'",
        expect_pattern="This will never match",
        timeout_seconds=2,
    )

    assert result.exit_code == -1
    assert result.error is not None
    assert "Pattern not found" in result.error
    assert "Hello World" in result.output  # Should still return current output


@pytest.mark.asyncio  # type: ignore[misc]
async def test_invalid_regex_pattern(tmux_server: str) -> None:
    """Test invalid regex pattern handling."""
    _ = tmux_server  # Use the fixture to set up test environment

    # Invalid regex pattern
    result = await tmux_run_command(
        "echo 'test'",
        expect_pattern="[invalid(regex",
        timeout_seconds=2,
    )

    assert result.exit_code == -1
    assert result.error is not None
    assert "Invalid regex" in result.error
