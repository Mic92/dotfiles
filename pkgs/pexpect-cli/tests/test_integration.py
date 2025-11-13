"""Integration tests for pexpect-cli."""

import json
import os
import platform
import shutil
import subprocess
import tempfile
import time
from collections.abc import Generator
from pathlib import Path

import pytest

# Use bin wrappers from the project
BIN_DIR = Path(__file__).parent.parent / "bin"
PEXPECT_CLI = [str(BIN_DIR / "pexpect-cli")]
PEXPECT_SERVER = [str(BIN_DIR / "pexpect-server")]


@pytest.fixture  # type: ignore[misc]
def isolated_pueue() -> Generator[dict[str, str]]:
    """Set up an isolated pueue instance for testing."""
    # Use /tmp on macOS to avoid long socket path issues
    base_tmp = "/tmp" if platform.system() == "Darwin" else None  # noqa: S108

    # Create temporary directory for pueue config and runtime
    tmpdir = Path(tempfile.mkdtemp(prefix="pexpect-test-", dir=base_tmp))

    try:
        pueue_dir = tmpdir / ".local" / "share" / "pueue"
        pueue_dir.mkdir(parents=True)

        # Set up environment for isolated pueue
        test_env = os.environ.copy()
        test_env["PATH"] = f"{BIN_DIR}:{test_env.get('PATH', '')}"
        test_env["PUEUE_CONFIG_DIR"] = str(pueue_dir)
        test_env["HOME"] = str(tmpdir)
        test_env["XDG_CACHE_HOME"] = str(tmpdir / ".cache")
        # Clear XDG_RUNTIME_DIR so we use XDG_CACHE_HOME
        test_env.pop("XDG_RUNTIME_DIR", None)

        # Start isolated pueued
        subprocess.Popen(
            ["pueued", "-d"],
            env=test_env,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )

        # Wait for daemon to be ready
        for _ in range(50):  # 5 second timeout
            result = subprocess.run(
                ["pueue", "status"],
                env=test_env,
                capture_output=True,
                check=False,
            )
            if result.returncode == 0:
                break
            time.sleep(0.1)
        else:
            msg = "Failed to start isolated pueue daemon"
            raise RuntimeError(msg)

        yield test_env

    finally:
        # Cleanup: kill daemon and remove temp directory
        subprocess.run(
            ["pueue", "shutdown"],
            env=test_env,
            capture_output=True,
            check=False,
        )
        time.sleep(0.5)
        shutil.rmtree(tmpdir, ignore_errors=True)


def test_full_workflow(isolated_pueue: dict[str, str]) -> None:
    """Test the complete workflow: start session, execute code, stop session."""
    # Start session
    result = subprocess.run(
        [*PEXPECT_CLI, "--start", "--name", "test-session"],
        capture_output=True,
        text=True,
        check=True,
        env=isolated_pueue,
    )
    session_id = result.stdout.strip()
    assert len(session_id) == 8, f"Expected 8-char hex session ID, got: {session_id}"
    assert all(c in "0123456789abcdef" for c in session_id), (
        f"Expected hex session ID, got: {session_id}"
    )

    try:
        # Wait for session to be ready
        time.sleep(1)

        # Execute code in session
        test_code = """
child = pexpect.spawn('bash')
child.sendline('echo hello')
child.expect('hello')
print("Got:", child.after.decode())
"""
        result = subprocess.run(
            [*PEXPECT_CLI, session_id],
            input=test_code,
            capture_output=True,
            text=True,
            check=True,
            env=isolated_pueue,
        )
        assert "Got: hello" in result.stdout

        # List sessions - should show our session
        result = subprocess.run(
            [*PEXPECT_CLI, "--list"],
            capture_output=True,
            text=True,
            check=True,
            env=isolated_pueue,
        )
        assert session_id in result.stdout
        assert "test-session" in result.stdout

        # Check pueue status shows a task with our session ID in the command
        result = subprocess.run(
            ["pueue", "status", "--json"],
            capture_output=True,
            text=True,
            check=True,
            env=isolated_pueue,
        )
        status_data = json.loads(result.stdout)

        # Find task with our session ID in the command
        found = False
        for task_id, task in status_data["tasks"].items():
            if session_id in task["command"]:
                assert "pexpect-server" in task["command"]
                found = True

                # View logs using the pueue task ID
                log_result = subprocess.run(
                    ["pueue", "log", task_id],
                    capture_output=True,
                    text=True,
                    check=True,
                    env=isolated_pueue,
                )
                assert "pexpect-server" in log_result.stdout
                assert session_id in log_result.stdout
                break

        assert found, f"Session {session_id} not found in pueue tasks"

    finally:
        # Stop session
        subprocess.run(
            [*PEXPECT_CLI, "--stop", session_id],
            check=False,
            env=isolated_pueue,
        )


def test_persistent_child_across_executions(isolated_pueue: dict[str, str]) -> None:
    """Test that child process persists across multiple executions."""
    # Start session
    result = subprocess.run(
        [*PEXPECT_CLI, "--start"],
        capture_output=True,
        text=True,
        check=True,
        env=isolated_pueue,
    )
    session_id = result.stdout.strip()

    try:
        time.sleep(1)

        # First execution: spawn bash
        code1 = """
child = pexpect.spawn('bash')
child.sendline('cd /tmp')
child.expect('\\$')
print("Spawned bash")
"""
        subprocess.run(
            [*PEXPECT_CLI, session_id],
            input=code1,
            capture_output=True,
            text=True,
            check=True,
            env=isolated_pueue,
        )

        # Second execution: use existing child
        code2 = """
if child and child.isalive():
    child.sendline('pwd')
    child.expect('\\$')
    output = child.before.decode()
    print("Current dir:", output)
else:
    print("ERROR: child not alive")
"""
        result = subprocess.run(
            [*PEXPECT_CLI, session_id],
            input=code2,
            capture_output=True,
            text=True,
            check=True,
            env=isolated_pueue,
        )
        assert "/tmp" in result.stdout  # noqa: S108
        assert "ERROR" not in result.stdout

    finally:
        subprocess.run(
            [*PEXPECT_CLI, "--stop", session_id],
            check=False,
            env=isolated_pueue,
        )


def test_oneshot_mode(isolated_pueue: dict[str, str]) -> None:
    """Test one-shot execution without session."""
    code = """
import pexpect
child = pexpect.spawn('echo', ['hello world'])
output = child.read().decode()
print("Output:", output.strip())
"""
    result = subprocess.run(
        PEXPECT_CLI,
        input=code,
        capture_output=True,
        text=True,
        check=True,
        env=isolated_pueue,
    )
    assert "Output: hello world" in result.stdout


def test_socket_location(isolated_pueue: dict[str, str]) -> None:
    """Test that socket is created in the correct XDG location."""
    result = subprocess.run(
        [*PEXPECT_CLI, "--start"],
        capture_output=True,
        text=True,
        check=False,
        env=isolated_pueue,
    )
    if result.returncode != 0:
        print(f"STDOUT: {result.stdout}")
        print(f"STDERR: {result.stderr}")
        msg = f"pexpect-cli --start failed with code {result.returncode}"
        raise RuntimeError(msg)
    session_id = result.stdout.strip()

    try:
        time.sleep(1)

        # Determine expected socket location using the test environment
        runtime_dir = isolated_pueue.get("XDG_RUNTIME_DIR")
        if runtime_dir:
            expected_dir = Path(runtime_dir) / "pexpect-cli"
        else:
            cache_home = isolated_pueue.get(
                "XDG_CACHE_HOME", Path(isolated_pueue["HOME"]) / ".cache"
            )
            expected_dir = Path(cache_home) / "pexpect-cli" / "sockets"

        sock_path = expected_dir / f"{session_id}.sock"
        assert sock_path.exists(), f"Socket not found at {sock_path}"
        assert sock_path.is_socket(), f"{sock_path} is not a socket"

    finally:
        subprocess.run(
            [*PEXPECT_CLI, "--stop", session_id],
            check=False,
            env=isolated_pueue,
        )


def test_server_requires_task_id(isolated_pueue: dict[str, str]) -> None:
    """Test that pexpect-server requires a task ID argument."""
    result = subprocess.run(
        PEXPECT_SERVER,
        capture_output=True,
        text=True,
        check=False,
        env=isolated_pueue,
    )
    assert result.returncode != 0
    assert "task_id" in result.stderr or "required" in result.stderr
