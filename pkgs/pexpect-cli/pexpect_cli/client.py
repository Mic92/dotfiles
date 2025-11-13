#!/usr/bin/env python3
"""pexpect-cli: Client for pexpect sessions."""

import argparse
import json
import os
import shutil
import socket
import subprocess
import sys
import time
import uuid
from contextlib import suppress
from pathlib import Path
from typing import Any

# Pueue group name for all pexpect sessions
PUEUE_GROUP = "pexpect"


def get_socket_path(task_id: str) -> Path:
    """Get socket path with XDG compliance."""
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR")
    if runtime_dir:
        sock_dir = Path(runtime_dir) / "pexpect-cli"
    else:
        cache_home = os.environ.get("XDG_CACHE_HOME", str(Path.home() / ".cache"))
        sock_dir = Path(cache_home) / "pexpect-cli" / "sockets"

    return sock_dir / f"{task_id}.sock"


def ensure_pueue_group() -> None:
    """Ensure the pexpect group exists in pueue with adequate parallelism."""
    result = subprocess.run(
        ["pueue", "group", "--json"],
        capture_output=True,
        text=True,
        check=True,
    )
    groups = json.loads(result.stdout)
    if PUEUE_GROUP not in groups:
        subprocess.run(
            ["pueue", "group", "add", PUEUE_GROUP],
            capture_output=True,
            check=True,
        )
        # Set parallelism to allow multiple sessions
        subprocess.run(
            ["pueue", "parallel", "--group", PUEUE_GROUP, "10"],
            capture_output=True,
            check=True,
        )


def get_pueue_tasks() -> dict[str, Any]:
    """Get all pueue tasks as a dictionary."""
    result = subprocess.run(
        ["pueue", "status", "--json"],
        capture_output=True,
        text=True,
        check=True,
    )
    data: dict[str, Any] = json.loads(result.stdout)
    return data


def start_session(name: str | None = None) -> str:
    """Start new pexpect server session."""
    server_path = shutil.which("pexpect-server")
    if not server_path:
        msg = "pexpect-server not found in PATH"
        raise RuntimeError(msg)

    # Ensure pexpect group exists
    ensure_pueue_group()

    # Generate a unique session ID that we control (not dependent on pueue's task ID)
    session_id = uuid.uuid4().hex[:8]

    # Pass necessary environment variables and session ID to server
    env_vars = [
        f"{var}={os.environ[var]}"
        for var in ["XDG_CACHE_HOME", "PUEUE_CONFIG_DIR"]
        if var in os.environ
    ]

    server_cmd = (
        ["env", *env_vars, server_path, session_id]
        if env_vars
        else [server_path, session_id]
    )

    cmd = ["pueue", "add", "--group", PUEUE_GROUP, "--", *server_cmd]
    if name:
        cmd.insert(2, "--label")
        cmd.insert(3, name)

    subprocess.run(cmd, capture_output=True, text=True, check=True)
    task_id = session_id

    sock_path = get_socket_path(task_id)

    # Wait for socket
    for _ in range(50):
        if sock_path.exists():
            print(task_id)
            return task_id
        time.sleep(0.1)

    # If timeout, include error details
    error_msg = f"Session {task_id} socket not created at {sock_path}"
    raise TimeoutError(error_msg)


def exec_in_session(session_id: str, code: str) -> None:
    """Execute code in existing session."""
    sock_path = get_socket_path(session_id)

    if not sock_path.exists():
        print(f"Error: Session {session_id} not found at {sock_path}", file=sys.stderr)
        sys.exit(1)

    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    try:
        sock.connect(str(sock_path))

        request = json.dumps({"code": code})
        sock.sendall(request.encode())

        # Signal end of request
        sock.shutdown(socket.SHUT_WR)

        # Read all response data until connection is closed
        response_data = b""
        while True:
            chunk = sock.recv(65536)
            if not chunk:
                break
            response_data += chunk

        response = json.loads(response_data.decode())

        if response["status"] == "error":
            print(f"Error: {response['error']}", file=sys.stderr)
            print(response["traceback"], file=sys.stderr)
            sys.exit(1)
        elif response["output"]:
            print(response["output"], end="")

    finally:
        sock.close()


def stop_session(session_id: str) -> None:
    """Stop session and cleanup socket."""
    # Find the pueue task ID for this session
    data = get_pueue_tasks()

    # Find task with this session ID in the command
    for task_id, task in data["tasks"].items():
        if session_id in task["command"]:
            subprocess.run(["pueue", "kill", task_id], check=False)
            break

    sock_path = get_socket_path(session_id)
    with suppress(FileNotFoundError):
        sock_path.unlink()


def list_sessions() -> None:
    """List active sessions."""
    data = get_pueue_tasks()

    for task_id, task in data["tasks"].items():
        if "pexpect-server" in task["command"]:
            # Extract session ID from command (last argument)
            cmd = task["command"]
            parts = cmd.split()
            session_id = parts[-1] if parts else task_id

            label = task.get("label", "")
            status = task["status"]
            # Format status nicely
            if isinstance(status, dict):
                status_str = next(iter(status.keys()))
            else:
                status_str = status

            print(f"{session_id}: {status_str}" + (f" ({label})" if label else ""))


def exec_oneshot(code: str) -> None:
    """Execute code without persistence."""
    import pexpect  # type: ignore[import-untyped]  # noqa: PLC0415

    namespace = {"pexpect": pexpect, "child": None}
    exec(code, namespace)  # noqa: S102


def main() -> None:
    """Entry point for pexpect-cli."""
    parser = argparse.ArgumentParser(
        description="pexpect-cli: Persistent pexpect sessions via pueue",
        epilog=f"""
Examples:
  # Start a new session
  session=$(pexpect-cli --start)

  # Execute code in session (from stdin)
  echo 'child = pexpect.spawn("bash")' | pexpect-cli $session

  # Using heredoc
  pexpect-cli $session <<EOF
  child.sendline('pwd')
  child.expect('$')
  print(child.before.decode())
  EOF

  # List all sessions
  pexpect-cli --list

  # Monitor session in real-time (all sessions run in the '{PUEUE_GROUP}' group)
  pueue status --group {PUEUE_GROUP}           # View all pexpect sessions
  pueue follow <task-id>                       # Watch output in real-time
  pueue log <task-id>                          # View full log

  # Stop session
  pexpect-cli --stop $session
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "session_id",
        nargs="?",
        help="Session ID to execute code in. Code is read from stdin.",
    )
    parser.add_argument(
        "--start",
        action="store_true",
        help="Start new persistent session. Returns session ID.",
    )
    parser.add_argument("--name", help="Optional label for the session")
    parser.add_argument("--stop", metavar="ID", help="Stop a session by ID")
    parser.add_argument("--list", action="store_true", help="List all active sessions")

    args = parser.parse_args()

    if args.start:
        start_session(args.name)
    elif args.stop:
        stop_session(args.stop)
    elif args.list:
        list_sessions()
    elif args.session_id:
        code = sys.stdin.read()
        exec_in_session(args.session_id, code)
    else:
        # One-shot mode
        code = sys.stdin.read()
        exec_oneshot(code)


if __name__ == "__main__":
    main()
