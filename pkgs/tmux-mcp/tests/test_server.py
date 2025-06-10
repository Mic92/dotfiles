"""Real-world integration tests for tmux MCP server high-level tools."""

import contextlib
import subprocess
import tempfile
from collections.abc import Generator
from pathlib import Path

import pytest
from tmux_mcp.server import (
    tmux_capture_pane,
    tmux_get_command_output,
    tmux_kill_pane,
    tmux_list_panes,
    tmux_list_sessions,
    tmux_ripgrep_command_output,
    tmux_run_command,
    tmux_send_input,
)


@pytest.fixture  # type: ignore[misc]
def tmux_server(monkeypatch: pytest.MonkeyPatch) -> Generator[str, None, None]:
    """Start a dedicated tmux server for testing."""
    # Clear tmux environment to ensure clean state
    monkeypatch.delenv("TMUX", raising=False)
    monkeypatch.delenv("TMUX_TMPDIR", raising=False)
    monkeypatch.delenv("TMUX_PANE", raising=False)

    # Create temporary socket directory
    with tempfile.TemporaryDirectory() as socket_dir:
        socket_path = f"{socket_dir}/tmux-test"

        # Set TMUX_TMPDIR to our test directory so tmux uses our socket
        monkeypatch.setenv("TMUX_TMPDIR", socket_dir)

        # Start tmux server with custom socket and capture the process
        proc = subprocess.Popen(
            ["tmux", "-S", socket_path, "new-session", "-d", "-s", "test-session"],
        )
        proc.wait()  # Wait for session creation to complete

        try:
            yield socket_path
        finally:
            # Cleanup - try graceful kill first, then force kill
            with contextlib.suppress(Exception):
                subprocess.run(
                    ["tmux", "-S", socket_path, "kill-server"], check=False, timeout=2
                )

            # Find and kill any remaining tmux processes using our socket
            with contextlib.suppress(Exception):
                result = subprocess.run(
                    ["pgrep", "-f", f"tmux.*{socket_path}"],
                    capture_output=True,
                    text=True,
                    timeout=2,
                    check=False,
                )
                if result.stdout.strip():
                    pids = result.stdout.strip().split("\n")
                    for pid in pids:
                        if pid:
                            subprocess.run(["kill", "-9", pid], check=False, timeout=1)


class TestTmuxIntegration:
    """Real-world integration tests using actual tmux."""

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_run_simple_command(self, tmux_server: str) -> None:
        """Test running a simple command in tmux."""
        _ = tmux_server  # Use the fixture to set up test environment
        result = await tmux_run_command("echo 'Hello World'", timeout_seconds=10)

        assert result.exit_code == 0
        assert "Hello World" in result.output
        assert result.pane_id.startswith("%")

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_run_command_with_working_dir(self, tmux_server: str) -> None:
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
    async def test_tmux_run_command_failure(self, tmux_server: str) -> None:
        """Test running a command that fails."""
        _ = tmux_server  # Use the fixture to set up test environment
        result = await tmux_run_command("nonexistent_command_12345", timeout_seconds=10)

        assert result.exit_code != 0
        assert result.pane_id.startswith("%")

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_list_sessions(self, tmux_server: str) -> None:
        """Test listing tmux sessions."""
        _ = tmux_server  # Use the fixture to set up test environment
        # Ensure we have at least one session by running a command first
        await tmux_run_command("echo 'setup session'", timeout_seconds=5)

        result = await tmux_list_sessions()
        assert "test-session" in result or "mcp" in result or "windows" in result

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_list_panes(self, tmux_server: str) -> None:
        """Test listing panes in a session."""
        _ = tmux_server  # Use the fixture to set up test environment
        # Create a pane first
        await tmux_run_command("echo 'create pane'", timeout_seconds=5)

        result = await tmux_list_panes()
        assert "%" in result  # Should contain pane IDs

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_send_input_and_capture(self, tmux_server: str) -> None:
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
        assert "Input sent" in send_result

        # Capture the pane output
        capture_result = await tmux_capture_pane(pane_id)
        assert isinstance(capture_result, str)

        # Kill the pane to clean up
        kill_result = await tmux_kill_pane(pane_id)
        assert "killed successfully" in kill_result

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_run_command_timeout(self, tmux_server: str) -> None:
        """Test command timeout behavior."""
        _ = tmux_server  # Use the fixture to set up test environment
        result = await tmux_run_command("sleep 10", timeout_seconds=2)

        assert result.exit_code == -1
        assert "timed out" in result.output
        assert result.error == "timeout"

        # No need to clean up the pane - it should be auto-cleaned due to timeout

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_multiple_commands_sequential(self, tmux_server: str) -> None:
        """Test running multiple commands in sequence."""
        _ = tmux_server  # Use the fixture to set up test environment
        # Run first command
        result1 = await tmux_run_command("echo 'First command'", timeout_seconds=5)
        assert result1.exit_code == 0

        # Run second command
        result2 = await tmux_run_command("echo 'Second command'", timeout_seconds=5)
        assert result2.exit_code == 0

        # Should have different pane IDs
        assert result1.pane_id != result2.pane_id

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_capture_pane_with_start_line(self, tmux_server: str) -> None:
        """Test capturing pane output with start line parameter."""
        _ = tmux_server  # Use the fixture to set up test environment
        # Run a command that produces multiple lines, keep pane for capture test
        result = await tmux_run_command(
            "echo 'Line 1'; echo 'Line 2'; echo 'Line 3'",
            timeout_seconds=5,
            keep_pane=True,
        )
        pane_id = result.pane_id

        # Capture from a specific start line
        capture_result = await tmux_capture_pane(pane_id, start_line=0)
        assert isinstance(capture_result, str)

        # Clean up
        await tmux_kill_pane(pane_id)

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_error_handling(self, tmux_server: str) -> None:
        """Test error handling with invalid pane operations."""
        _ = tmux_server  # Use the fixture to set up test environment
        # Try to send input to non-existent pane
        result = await tmux_send_input("%999", "test input")
        assert "Error" in result

        # Try to capture from non-existent pane
        result = await tmux_capture_pane("%999")
        assert "Error" in result

        # Try to kill non-existent pane
        result = await tmux_kill_pane("%999")
        assert "Error" in result

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_automatic_pane_cleanup(self, tmux_server: str) -> None:
        """Test that panes are automatically cleaned up after command completion."""
        _ = tmux_server  # Use the fixture to set up test environment

        # Run a command (should auto-close pane by default)
        result = await tmux_run_command("echo 'test cleanup'", timeout_seconds=5)
        assert result.exit_code == 0
        pane_id = result.pane_id

        # Verify the pane is automatically gone by trying to capture it
        # This should fail because the pane was auto-closed
        capture_result = await tmux_capture_pane(pane_id)
        assert "Error" in capture_result

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_keep_pane_option(self, tmux_server: str) -> None:
        """Test that panes can be kept open when keep_pane=True."""
        _ = tmux_server  # Use the fixture to set up test environment

        # Get initial pane count
        initial_panes = await tmux_list_panes()
        initial_count = initial_panes.count("%")

        # Run a command with keep_pane=True
        result = await tmux_run_command(
            "echo 'test keep pane'", timeout_seconds=5, keep_pane=True
        )
        assert result.exit_code == 0
        pane_id = result.pane_id

        # Check that pane is still there
        final_panes = await tmux_list_panes()
        final_count = final_panes.count("%")

        # Should have one more pane now
        assert final_count == initial_count + 1

        # Verify the pane is still accessible
        capture_result = await tmux_capture_pane(pane_id)
        assert "Error" not in capture_result
        assert "test keep pane" in capture_result

        # Clean up manually
        await tmux_kill_pane(pane_id)

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_pagination_basic(self, tmux_server: str) -> None:
        """Test basic pagination functionality."""
        _ = tmux_server  # Use the fixture to set up test environment

        # Generate multi-line output for pagination testing
        result = await tmux_run_command(
            'for i in {1..150}; do echo "Line $i: This is line number $i"; done',
            timeout_seconds=10,
        )

        assert result.exit_code == 0
        pane_id = result.pane_id

        # Check that pagination info is present in initial response (first 100 lines)
        assert result.pagination is not None
        pagination = result.pagination
        assert pagination.total_lines == 150
        assert pagination.displayed_lines == 100
        assert pagination.start_line == 0
        assert pagination.next_cursor is not None

        # Get next page using cursor
        next_cursor = pagination.next_cursor
        next_page = await tmux_get_command_output(pane_id, next_cursor)

        assert next_page.error is None
        assert next_page.pane_id == pane_id
        assert next_page.pagination is not None
        next_pagination = next_page.pagination
        assert next_pagination.displayed_lines == 50  # Remaining lines
        assert next_pagination.start_line == 100
        assert next_pagination.next_cursor is None  # No more pages

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_pagination_custom_cursor(self, tmux_server: str) -> None:
        """Test pagination with custom cursor."""
        _ = tmux_server  # Use the fixture to set up test environment

        # Generate output
        result = await tmux_run_command(
            'for i in {1..50}; do echo "Test line $i"; done',
            timeout_seconds=10,
        )
        pane_id = result.pane_id

        # Get specific range: lines 10-19 (10 lines starting from line 10)
        page = await tmux_get_command_output(pane_id, "10:10")

        assert page.error is None
        assert page.pagination is not None
        pagination = page.pagination
        assert pagination.displayed_lines == 10
        assert pagination.start_line == 10
        assert "Test line 11" in page.output  # Line 11 (index 10)
        assert "Test line 20" in page.output  # Line 20 (index 19)

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_pagination_invalid_pane(self, tmux_server: str) -> None:
        """Test pagination with invalid pane ID."""
        _ = tmux_server  # Use the fixture to set up test environment

        # Try to get output from non-existent pane
        result = await tmux_get_command_output("%999", "0:10")

        assert result.error is not None
        assert "No cached output found" in result.error
        assert result.pane_id == "%999"

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_pagination_no_cursor(self, tmux_server: str) -> None:
        """Test getting full output without pagination."""
        _ = tmux_server  # Use the fixture to set up test environment

        # Generate small output
        result = await tmux_run_command(
            'for i in {1..10}; do echo "Line $i"; done',
            timeout_seconds=10,
        )
        pane_id = result.pane_id

        # Get all output without cursor (no pagination)
        full_output = await tmux_get_command_output(pane_id)

        assert full_output.error is None
        assert full_output.pagination is not None
        pagination = full_output.pagination
        assert pagination.total_lines == 10
        assert pagination.displayed_lines == 10
        assert pagination.start_line == 0
        assert pagination.next_cursor is None  # No pagination applied

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_ripgrep_command_output_basic(self, tmux_server: str) -> None:
        """Test basic ripgrep functionality on cached command output."""
        _ = tmux_server  # Use the fixture to set up test environment

        # Generate output with specific patterns
        result = await tmux_run_command(
            'echo "ERROR: Something went wrong"; echo "INFO: Everything is fine"; echo "DEBUG: More details"',
            timeout_seconds=10,
        )
        pane_id = result.pane_id

        # Search for ERROR pattern
        rg_result = await tmux_ripgrep_command_output(pane_id, "ERROR")

        assert rg_result.error is None
        assert rg_result.exit_code == 0
        assert "ERROR: Something went wrong" in rg_result.output
        assert "INFO:" not in rg_result.output
        assert rg_result.command == "rg ERROR"

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_ripgrep_command_output_with_flags(
        self, tmux_server: str
    ) -> None:
        """Test ripgrep with flags on cached command output."""
        _ = tmux_server  # Use the fixture to set up test environment

        # Generate output with specific patterns
        result = await tmux_run_command(
            'echo "error: lowercase error"; echo "ERROR: uppercase error"; echo "Info: mixed case"',
            timeout_seconds=10,
        )
        pane_id = result.pane_id

        # Search case-insensitively with line numbers
        rg_result = await tmux_ripgrep_command_output(pane_id, "error", "-i -n")

        assert rg_result.error is None
        assert rg_result.exit_code == 0
        assert "error: lowercase error" in rg_result.output
        assert "ERROR: uppercase error" in rg_result.output
        assert "Info:" not in rg_result.output
        assert rg_result.command == "rg -i -n error"

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_ripgrep_command_output_no_matches(
        self, tmux_server: str
    ) -> None:
        """Test ripgrep when no matches are found."""
        _ = tmux_server  # Use the fixture to set up test environment

        # Generate output without the search pattern
        result = await tmux_run_command(
            'echo "Line 1"; echo "Line 2"; echo "Line 3"',
            timeout_seconds=10,
        )
        pane_id = result.pane_id

        # Search for non-existent pattern
        rg_result = await tmux_ripgrep_command_output(pane_id, "NOTFOUND")

        assert rg_result.error is None
        assert rg_result.exit_code == 1
        assert "No matches found for pattern: NOTFOUND" in rg_result.output

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_ripgrep_command_output_invalid_pane(
        self, tmux_server: str
    ) -> None:
        """Test ripgrep with invalid pane ID."""
        _ = tmux_server  # Use the fixture to set up test environment

        # Try to search in non-existent pane
        rg_result = await tmux_ripgrep_command_output("%999", "test")

        assert rg_result.error is not None
        assert "No cached output found" in rg_result.error
        assert rg_result.pane_id == "%999"
        assert rg_result.exit_code == -1

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_ripgrep_pagination_basic(self, tmux_server: str) -> None:
        """Test basic ripgrep pagination functionality."""
        _ = tmux_server  # Use the fixture to set up test environment

        # Generate output with many matching lines
        result = await tmux_run_command(
            'for i in {1..150}; do echo "Match line $i: ERROR found"; done',
            timeout_seconds=10,
        )
        pane_id = result.pane_id

        # Search with pagination (first 10 matches)
        rg_result = await tmux_ripgrep_command_output(pane_id, "ERROR", cursor="0:10")

        assert rg_result.error is None
        assert rg_result.exit_code == 0
        assert rg_result.pagination is not None
        pagination = rg_result.pagination
        assert pagination.total_lines == 150
        assert pagination.displayed_lines == 10
        assert pagination.start_line == 0
        assert pagination.next_cursor == "10:10"
        assert "Match line 1: ERROR found" in rg_result.output
        assert "Match line 10: ERROR found" in rg_result.output
        assert "Match line 11: ERROR found" not in rg_result.output

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_ripgrep_pagination_next_page(self, tmux_server: str) -> None:
        """Test ripgrep pagination with next page."""
        _ = tmux_server  # Use the fixture to set up test environment

        # Generate output
        result = await tmux_run_command(
            'for i in {1..50}; do echo "Line $i: DEBUG message"; done',
            timeout_seconds=10,
        )
        pane_id = result.pane_id

        # Get first page
        first_page = await tmux_ripgrep_command_output(pane_id, "DEBUG", cursor="0:20")
        assert first_page.pagination is not None
        assert first_page.pagination.displayed_lines == 20

        # Get second page using cursor
        next_cursor = first_page.pagination.next_cursor
        second_page = await tmux_ripgrep_command_output(
            pane_id, "DEBUG", cursor=next_cursor
        )

        assert second_page.error is None
        assert second_page.pagination is not None
        assert second_page.pagination.displayed_lines == 20
        assert second_page.pagination.start_line == 20
        assert "Line 21: DEBUG message" in second_page.output
        assert "Line 40: DEBUG message" in second_page.output

    @pytest.mark.asyncio  # type: ignore[misc]
    async def test_tmux_ripgrep_no_pagination(self, tmux_server: str) -> None:
        """Test ripgrep without pagination returns all matches."""
        _ = tmux_server  # Use the fixture to set up test environment

        # Generate output with few matches
        result = await tmux_run_command(
            'echo "Line 1: INFO"; echo "Line 2: ERROR"; echo "Line 3: INFO"; echo "Line 4: ERROR"',
            timeout_seconds=10,
        )
        pane_id = result.pane_id

        # Search without cursor (no pagination)
        rg_result = await tmux_ripgrep_command_output(pane_id, "ERROR")

        assert rg_result.error is None
        assert rg_result.exit_code == 0
        assert "Line 2: ERROR" in rg_result.output
        assert "Line 4: ERROR" in rg_result.output
        # When no pagination cursor is provided, pagination info should not be included
        assert rg_result.pagination is None
