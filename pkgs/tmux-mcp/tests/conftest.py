"""Shared fixtures for tmux MCP tests."""

import contextlib
import subprocess
import tempfile
from collections.abc import Generator
from typing import Any

import pytest

# Global timeout for all async tests (in seconds)
GLOBAL_TEST_TIMEOUT = 60


def pytest_configure(config: Any) -> None:
    """Configure pytest with custom markers and settings."""
    config.addinivalue_line(
        "markers", "timeout: mark test to set custom timeout duration"
    )


@pytest.fixture(autouse=True)
def async_test_timeout(request: Any) -> None:
    """Apply global timeout to all async tests."""
    if request.node.get_closest_marker("asyncio"):
        # Check if test has a custom timeout marker
        timeout_marker = request.node.get_closest_marker("timeout")
        if timeout_marker and timeout_marker.args:
            timeout = timeout_marker.args[0]
        else:
            timeout = GLOBAL_TEST_TIMEOUT

        # Apply timeout to the test
        request.node.add_marker(pytest.mark.timeout(timeout))


@pytest.fixture
def tmux_server(monkeypatch: pytest.MonkeyPatch) -> Generator[str]:
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
