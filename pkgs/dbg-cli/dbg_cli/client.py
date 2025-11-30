#!/usr/bin/env python3
"""Debugger CLI client - sends Python code to debugger server.

This is the main entry point for LLM agents and users.
Python code is read from stdin and executed in a namespace with `dbg` available.
Responses are JSON on stdout.

Example:
    echo 'dbg.launch("/bin/ls")' | dbg-cli SESSION_ID
    echo 'dbg.breakpoint("main")' | dbg-cli SESSION_ID
    echo 'dbg.continue_()' | dbg-cli SESSION_ID
"""

from __future__ import annotations

import argparse
import json
import os
import socket
import subprocess
import sys
import time
import uuid
from contextlib import suppress
from pathlib import Path
from typing import Any

from dbg_cli.server import get_socket_dir, get_socket_path

# Buffer size for socket communication
BUFFER_SIZE = 65536

# Pueue group for debugger sessions
PUEUE_GROUP = "dbg"


def generate_session_id() -> str:
    """Generate a unique session ID."""
    return uuid.uuid4().hex[:8]


def ensure_pueue_group() -> bool:
    """Ensure the pueue group exists. Returns True if successful."""
    try:
        # Check if pueue daemon is running
        result = subprocess.run(
            ["pueue", "status", "--json"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if result.returncode != 0:
            # Try to start pueue daemon
            subprocess.run(["pueue", "-d"], capture_output=True, timeout=5)
            time.sleep(0.5)

        # Check if group exists
        result = subprocess.run(
            ["pueue", "group"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if PUEUE_GROUP not in result.stdout:
            # Create group
            subprocess.run(
                ["pueue", "group", "add", PUEUE_GROUP],
                capture_output=True,
                timeout=5,
            )
            # Set parallel limit
            subprocess.run(
                ["pueue", "parallel", "10", "--group", PUEUE_GROUP],
                capture_output=True,
                timeout=5,
            )

        return True

    except (subprocess.TimeoutExpired, FileNotFoundError):
        return False


def start_session(backend: str = "lldb", name: str | None = None) -> str | None:
    """Start a new debugger session via pueue.

    Returns session ID or None on failure.
    """
    if not ensure_pueue_group():
        print('{"error": "pueue not available"}', file=sys.stderr)
        return None

    session_id = generate_session_id()
    label = name or session_id

    # Start server via pueue
    cmd = [
        "pueue",
        "add",
        "--group",
        PUEUE_GROUP,
        "--label",
        label,
        "--print-task-id",
        "--",
        "dbg-server",
        session_id,
        "--backend",
        backend,
    ]

    try:
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
        if result.returncode != 0:
            print(f'{{"error": "Failed to start session: {result.stderr}"}}', file=sys.stderr)
            return None

        # Wait for socket to be created
        socket_path = get_socket_path(session_id)
        for _ in range(50):  # 5 second timeout
            if socket_path.exists():
                return session_id
            time.sleep(0.1)

        print('{"error": "Session startup timeout"}', file=sys.stderr)
        return None

    except subprocess.TimeoutExpired:
        print('{"error": "pueue timeout"}', file=sys.stderr)
        return None


def stop_session(session_id: str) -> bool:
    """Stop a debugger session."""
    # Send quit command first
    try:
        send_code(session_id, "dbg.quit()")
    except Exception:
        pass

    # Also try to kill via pueue using label
    try:
        # Find task ID by label
        result = subprocess.run(
            ["pueue", "status", "--json", "--group", PUEUE_GROUP],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if result.returncode == 0:
            status = json.loads(result.stdout)
            tasks = status.get("tasks", {})
            for task_id, task in tasks.items():
                if session_id in task.get("label", ""):
                    subprocess.run(
                        ["pueue", "kill", str(task_id)],
                        capture_output=True,
                        timeout=5,
                    )
                    subprocess.run(
                        ["pueue", "remove", str(task_id)],
                        capture_output=True,
                        timeout=5,
                    )
                    break

    except Exception:
        pass

    # Clean up socket
    socket_path = get_socket_path(session_id)
    if socket_path.exists():
        with suppress(Exception):
            socket_path.unlink()

    return True


def list_sessions() -> list[dict[str, Any]]:
    """List active debugger sessions."""
    sessions: list[dict[str, Any]] = []

    try:
        result = subprocess.run(
            ["pueue", "status", "--json", "--group", PUEUE_GROUP],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if result.returncode == 0:
            status = json.loads(result.stdout)
            tasks = status.get("tasks", {})
            for task_id, task in tasks.items():
                # Extract session ID from command
                command = task.get("command", "")
                label = task.get("label", "")

                # Find session ID in command args
                parts = command.split()
                session_id = None
                for i, part in enumerate(parts):
                    if part == "dbg-server" and i + 1 < len(parts):
                        session_id = parts[i + 1]
                        break

                if session_id:
                    sessions.append({
                        "session_id": session_id,
                        "label": label,
                        "status": task.get("status", "unknown"),
                        "task_id": task_id,
                    })

    except Exception:
        pass

    # Also check for orphaned sockets
    socket_dir = get_socket_dir()
    for sock_file in socket_dir.glob("*.sock"):
        session_id = sock_file.stem
        if not any(s["session_id"] == session_id for s in sessions):
            sessions.append({
                "session_id": session_id,
                "label": session_id,
                "status": "orphaned",
                "task_id": None,
            })

    return sessions


def send_code(session_id: str, code: str) -> dict[str, Any]:
    """Send Python code to a session and return the response."""
    socket_path = get_socket_path(session_id)

    if not socket_path.exists():
        return {"status": "error", "error": f"Session {session_id} not found"}

    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.settimeout(30.0)  # 30 second timeout for long operations
        sock.connect(str(socket_path))

        # Send code
        request = {"code": code}
        sock.sendall(json.dumps(request).encode())
        sock.shutdown(socket.SHUT_WR)

        # Read response
        data = b""
        while True:
            chunk = sock.recv(BUFFER_SIZE)
            if not chunk:
                break
            data += chunk

        sock.close()

        if data:
            return json.loads(data.decode())
        else:
            return {"status": "error", "error": "Empty response from server"}

    except socket.timeout:
        return {"status": "error", "error": "Code execution timeout"}
    except ConnectionRefusedError:
        return {"status": "error", "error": f"Cannot connect to session {session_id}"}
    except json.JSONDecodeError as e:
        return {"status": "error", "error": f"Invalid response: {e}"}
    except Exception as e:
        return {"status": "error", "error": str(e)}


def main() -> None:
    """Main entry point for dbg-cli."""
    parser = argparse.ArgumentParser(
        description="dbg-cli - LLM-optimized debugger interface (Python code via stdin)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Start a new session
  dbg-cli --start --backend lldb

  # Send Python code to a session
  echo 'dbg.launch("/path/to/binary")' | dbg-cli SESSION_ID
  echo 'dbg.breakpoint("main")' | dbg-cli SESSION_ID
  echo 'dbg.continue_()' | dbg-cli SESSION_ID
  echo 'dbg.locals()' | dbg-cli SESSION_ID

  # Multi-line code
  dbg-cli SESSION_ID << 'EOF'
  result = dbg.backtrace()
  for frame in result["result"]["frames"]:
      print(f"{frame['index']}: {frame['function']}")
  EOF

  # List sessions
  dbg-cli --list

  # Stop a session
  dbg-cli --stop SESSION_ID

Available in namespace:
  dbg           - Debugger interface with methods:
                  launch, attach, detach, quit
                  continue_, step, next, finish
                  reverse_continue, reverse_step (RR only)
                  breakpoint, delete, breakpoints, watch
                  backtrace, frame, thread, threads
                  locals, args, print, memory, registers
                  source, disassemble, status
  json          - json module
  print         - print function
""",
    )

    parser.add_argument(
        "session_id",
        nargs="?",
        help="Session ID to send code to",
    )
    parser.add_argument(
        "--start",
        "-s",
        action="store_true",
        help="Start a new debugger session",
    )
    parser.add_argument(
        "--stop",
        "-k",
        metavar="SESSION",
        help="Stop a debugger session",
    )
    parser.add_argument(
        "--list",
        "-l",
        action="store_true",
        help="List active sessions",
    )
    parser.add_argument(
        "--backend",
        "-b",
        choices=["lldb", "rr"],
        default="lldb",
        help="Debugger backend (for --start)",
    )
    parser.add_argument(
        "--name",
        "-n",
        help="Session name/label (for --start)",
    )
    parser.add_argument(
        "--code",
        "-c",
        help="Python code to execute (alternative to stdin)",
    )

    args = parser.parse_args()

    # Handle --start
    if args.start:
        session_id = start_session(args.backend, args.name)
        if session_id:
            print(json.dumps({"session_id": session_id, "backend": args.backend}))
            sys.exit(0)
        else:
            sys.exit(1)

    # Handle --stop
    if args.stop:
        success = stop_session(args.stop)
        print(json.dumps({"stopped": args.stop, "success": success}))
        sys.exit(0 if success else 1)

    # Handle --list
    if args.list:
        sessions = list_sessions()
        print(json.dumps({"sessions": sessions}))
        sys.exit(0)

    # Handle code execution
    if args.session_id:
        # Get code from --code or stdin
        if args.code:
            code = args.code
        elif not sys.stdin.isatty():
            code = sys.stdin.read()
        else:
            parser.error("No code provided. Use --code or pipe via stdin.")
            sys.exit(1)

        if not code.strip():
            parser.error("Empty code")
            sys.exit(1)

        # Send code and print response
        response = send_code(args.session_id, code)
        print(json.dumps(response, indent=2))

        # Exit with error code if response indicates error
        if response.get("status") == "error":
            sys.exit(1)
        sys.exit(0)

    # No action specified
    parser.print_help()
    sys.exit(1)


if __name__ == "__main__":
    main()
