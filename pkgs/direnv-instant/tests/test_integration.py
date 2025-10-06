"""Integration tests for direnv-instant."""

from __future__ import annotations

import multiprocessing
import os
import signal
import socket as sock_module
import subprocess
import time
from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from _pytest.monkeypatch import MonkeyPatch

PROJECT_ROOT = Path(__file__).parent.parent


def wait_for_sigusr1(queue: multiprocessing.Queue, timeout: int) -> None:
    """Subprocess that waits for SIGUSR1 and reports back."""
    received = False

    def handler(signum: int, frame: object) -> None:
        nonlocal received
        received = True

    signal.signal(signal.SIGUSR1, handler)

    # Wait for signal
    start = time.time()
    while not received and (time.time() - start) < timeout:
        time.sleep(0.1)

    queue.put(received)


def get_direnv_instant_binary() -> list[str]:
    """Get the direnv-instant command from env or fallback to cargo run."""
    if binary := os.environ.get("DIRENV_INSTANT_BIN"):
        # Resolve relative paths against PROJECT_ROOT, not cwd
        binary_path = Path(binary)
        if not binary_path.is_absolute():
            binary_path = PROJECT_ROOT / binary_path
        return [str(binary_path.absolute())]

    # Fallback to cargo run
    return [
        "cargo",
        "run",
        "--quiet",
        "--manifest-path",
        str(PROJECT_ROOT / "Cargo.toml"),
        "--",
    ]


def run_direnv_instant(
    args: list[str], env: dict[str, str]
) -> subprocess.CompletedProcess[str]:
    """Run direnv-instant with given args and environment."""
    cmd = get_direnv_instant_binary() + args

    return subprocess.run(
        cmd,
        check=False,
        env=env,
        capture_output=True,
        text=True,
    )


def setup_envrc(tmp_path: Path, content: str) -> Path:
    """Create and configure .envrc file."""
    envrc = tmp_path / ".envrc"
    envrc.write_text(content)
    envrc.chmod(0o755)
    return envrc


def setup_stub_tmux(tmp_path: Path, script_content: str | None = None) -> Path:
    """Create stub tmux script."""
    stub_tmux = tmp_path / "tmux"
    if script_content is None:
        script_content = "#!/bin/bash\nexit 0\n"
    stub_tmux.write_text(script_content)
    stub_tmux.chmod(0o755)
    return stub_tmux


def setup_test_env(
    tmp_path: Path, shell_pid: int, tmux_delay: str = "1"
) -> dict[str, str]:
    """Prepare environment for direnv-instant tests."""
    env = os.environ.copy()
    env["TMUX"] = "test"
    env["DIRENV_INSTANT_SHELL_PID"] = str(shell_pid)
    env["DIRENV_INSTANT_TMUX_DELAY"] = tmux_delay
    env["PATH"] = f"{tmp_path}:{env['PATH']}"
    return env


def allow_direnv(tmp_path: Path, monkeypatch: MonkeyPatch) -> None:
    """Change to test directory and allow direnv."""
    monkeypatch.chdir(tmp_path)
    subprocess.run(["direnv", "allow"], check=True, capture_output=True)


def test_blocking_envrc_calls_tmux(tmp_path: Path, monkeypatch: MonkeyPatch) -> None:
    """Test that direnv-instant calls tmux even when direnv blocks forever."""
    setup_envrc(tmp_path, "sleep 3600\n")

    tmux_called_file = tmp_path / "tmux_called"
    setup_stub_tmux(
        tmp_path,
        f"""#!/bin/bash
touch {tmux_called_file}
echo "$@" > {tmp_path / "tmux_args"}
""",
    )

    allow_direnv(tmp_path, monkeypatch)

    queue: multiprocessing.Queue[bool] = multiprocessing.Queue()
    signal_process = multiprocessing.Process(target=wait_for_sigusr1, args=(queue, 5))
    signal_process.start()

    pid = signal_process.pid
    assert pid is not None, "Failed to get PID of signal process"

    env = setup_test_env(tmp_path, pid)

    # Run direnv-instant start (should not block)
    start_time = time.time()
    result = run_direnv_instant(["start"], env)
    elapsed = time.time() - start_time

    # Should complete quickly (not block forever)
    assert elapsed < 3, f"direnv-instant blocked for {elapsed}s"
    assert result.returncode == 0, f"Failed: {result.stderr}"

    # Check that it exported the env vars
    assert "__DIRENV_INSTANT_ENV_FILE" in result.stdout
    assert "__DIRENV_INSTANT_STDERR_FILE" in result.stdout
    assert "__DIRENV_INSTANT_CURRENT_DIR" in result.stdout

    # Wait for tmux to be called (with some buffer beyond the delay)
    time.sleep(2)

    # Verify tmux was called
    assert tmux_called_file.exists(), "tmux stub was not called"

    # Verify tmux was called with correct arguments
    tmux_args_file = tmp_path / "tmux_args"
    assert tmux_args_file.exists(), "tmux args file not found"
    tmux_args = tmux_args_file.read_text().strip()
    assert "split-window" in tmux_args
    assert "direnv-instant" in tmux_args
    assert "watch" in tmux_args

    # Clean up signal process
    signal_process.join(timeout=1)
    signal_process.terminate()


def test_slow_direnv_exports_via_tmux(tmp_path: Path, monkeypatch: MonkeyPatch) -> None:
    """Test that direnv exports environment variables even when it takes longer than DIRENV_INSTANT_TMUX_DELAY."""
    setup_envrc(
        tmp_path,
        """#!/bin/bash
echo "Starting build..." >&2
sleep 1
echo "Still building..." >&2
sleep 1
echo "Almost done..." >&2
sleep 1
echo "Build complete!" >&2
export FOO=bar
export BAZ=qux
""",
    )

    tmux_called_file = tmp_path / "tmux_called"
    watch_output_file = tmp_path / "watch_output"
    direnv_instant_cmd = " ".join(get_direnv_instant_binary())

    setup_stub_tmux(
        tmp_path,
        f"""#!/bin/bash
touch {tmux_called_file}
log_path="${{@: -2:1}}"
socket_path="${{@: -1}}"
{direnv_instant_cmd} watch "$log_path" "$socket_path" > {watch_output_file} 2>&1 &
""",
    )

    allow_direnv(tmp_path, monkeypatch)

    queue: multiprocessing.Queue[bool] = multiprocessing.Queue()
    signal_process = multiprocessing.Process(target=wait_for_sigusr1, args=(queue, 10))
    signal_process.start()

    env = setup_test_env(tmp_path, signal_process.pid)

    # Run direnv-instant start (should not block)
    start_time = time.time()
    result = run_direnv_instant(["start"], env)
    elapsed = time.time() - start_time

    # Should complete quickly (not wait for direnv to finish)
    assert elapsed < 2, f"direnv-instant blocked for {elapsed}s"
    assert result.returncode == 0, f"Failed: {result.stderr}"

    # Parse env file path from output
    env_file_path = None
    for line in result.stdout.splitlines():
        if "__DIRENV_INSTANT_ENV_FILE" in line:
            # Extract path from: export __DIRENV_INSTANT_ENV_FILE="/path/to/file"
            env_file_path = line.split("=", 1)[1].strip().strip('"')
            break
    assert env_file_path, "Could not find __DIRENV_INSTANT_ENV_FILE in output"

    # Wait for tmux to be called (after 1s delay)
    time.sleep(2)
    assert tmux_called_file.exists(), "tmux stub was not called"

    # Wait for direnv to complete and write env file (3s execution + processing time)
    timeout_val = 6
    start = time.time()
    env_file = Path(env_file_path)
    while not env_file.exists() and (time.time() - start) < timeout_val:
        time.sleep(0.1)

    assert env_file.exists(), f"Env file not created after {timeout_val}s"

    # Verify environment variables were exported
    env_content = env_file.read_text()
    assert "FOO" in env_content, "FOO not found in env file"
    assert "bar" in env_content, "bar not found in env file"
    assert "BAZ" in env_content, "BAZ not found in env file"
    assert "qux" in env_content, "qux not found in env file"

    # Verify SIGUSR1 was received
    signal_process.join(timeout=1)
    assert not queue.empty(), "SIGUSR1 signal queue is empty"
    signal_received = queue.get()
    assert signal_received, "SIGUSR1 was not received"

    # Wait for watch output to be captured
    timeout_watch = 5
    start = time.time()
    while (time.time() - start) < timeout_watch:
        if watch_output_file.exists() and watch_output_file.stat().st_size > 0:
            break
        time.sleep(0.1)

    # Verify watch command was invoked and captured output
    assert watch_output_file.exists(), "watch output file was not created"
    watch_output = watch_output_file.read_text()
    assert len(watch_output) > 0, "watch command produced no output"
    # Verify we captured direnv output
    assert "Starting build" in watch_output or "direnv" in watch_output.lower()

    # Clean up
    signal_process.terminate()


def test_no_tmux_runs_direnv_synchronously(
    tmp_path: Path, monkeypatch: MonkeyPatch
) -> None:
    """Test that direnv-instant runs direnv synchronously when not in tmux."""
    setup_envrc(
        tmp_path,
        """#!/bin/bash
sleep 1
export SYNC_TEST=success
""",
    )

    allow_direnv(tmp_path, monkeypatch)

    # Prepare environment WITHOUT TMUX
    env = os.environ.copy()
    env.pop("TMUX", None)  # Ensure TMUX is not set

    # Run direnv-instant start (should block until direnv completes)
    start_time = time.time()
    result = run_direnv_instant(["start"], env)
    elapsed = time.time() - start_time

    # Should block for at least the sleep duration
    assert elapsed >= 1, f"direnv-instant returned too quickly: {elapsed}s"
    assert result.returncode == 0, f"Failed: {result.stderr}"

    # Should output direnv's export statements directly
    assert "SYNC_TEST" in result.stdout or "SYNC_TEST" in result.stderr

    # Should not set daemon-related env vars
    assert "__DIRENV_INSTANT_ENV_FILE" not in result.stdout
    assert "__DIRENV_INSTANT_STDERR_FILE" not in result.stdout
    assert "__DIRENV_INSTANT_CURRENT_DIR" in result.stdout  # This is always set


def test_stop_command_stops_daemon(tmp_path: Path, monkeypatch: MonkeyPatch) -> None:
    """Test that the stop command properly stops the running daemon."""
    setup_envrc(tmp_path, "sleep 3600\n")
    setup_stub_tmux(tmp_path)
    allow_direnv(tmp_path, monkeypatch)

    env = setup_test_env(tmp_path, os.getpid(), tmux_delay="60")

    result = run_direnv_instant(["start"], env)
    assert result.returncode == 0

    # Get socket path from stderr file path (in same directory)
    stderr_file = None
    for line in result.stdout.splitlines():
        if "__DIRENV_INSTANT_STDERR_FILE" in line:
            stderr_file = Path(line.split("=", 1)[1].strip().strip('"'))
            break
    assert stderr_file, "Could not find stderr file path"
    socket_path = stderr_file.parent / "daemon.sock"

    # Wait for daemon socket
    for _ in range(30):
        if socket_path.exists():
            break
        time.sleep(0.1)
    assert socket_path.exists(), "Daemon socket not created"

    # Run stop command
    env["__DIRENV_INSTANT_CURRENT_DIR"] = str(tmp_path)
    stop_result = run_direnv_instant(["stop"], env)
    assert stop_result.returncode == 0

    # Verify socket is gone or not accepting connections
    time.sleep(1)
    try:
        sock = sock_module.socket(sock_module.AF_UNIX, sock_module.SOCK_STREAM)
        sock.settimeout(1)
        sock.connect(str(socket_path))
        sock.close()
    except (OSError, TimeoutError):
        pass  # Expected - daemon stopped
    else:
        msg = "Daemon still running after stop"
        raise AssertionError(msg)
