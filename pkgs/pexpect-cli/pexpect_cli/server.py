#!/usr/bin/env python3
"""pexpect-server: Long-running pexpect session server."""

import argparse
import io
import json
import os
import signal
import socket
import sys
import time
import traceback
from contextlib import suppress
from pathlib import Path
from typing import Any

import pexpect  # type: ignore[import-untyped]


def get_socket_path(task_id: str) -> Path:
    """Get socket path with XDG compliance."""
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR")
    if runtime_dir:
        sock_dir = Path(runtime_dir) / "pexpect-cli"
    else:
        cache_home = os.environ.get("XDG_CACHE_HOME", str(Path.home() / ".cache"))
        sock_dir = Path(cache_home) / "pexpect-cli" / "sockets"

    sock_dir.mkdir(mode=0o700, parents=True, exist_ok=True)
    return sock_dir / f"{task_id}.sock"


def cleanup_child(child: Any) -> None:
    """Terminate a child process if it exists."""
    if child is not None and hasattr(child, "isalive") and child.isalive():
        with suppress(Exception):
            child.terminate(force=True)


def handle_request(conn: socket.socket, namespace: dict[str, Any]) -> None:  # noqa: C901
    """Handle a single client request."""
    timestamp = time.strftime("%H:%M:%S")

    try:
        # Receive all data until client closes its write end
        data_parts = []
        while True:
            chunk = conn.recv(65536)
            if not chunk:
                break
            data_parts.append(chunk)

        data = b"".join(data_parts).decode()
        request = json.loads(data)
        code = request["code"]

        print(f"\n[{timestamp}] Executing:")
        print(code)
        sys.stdout.flush()

        # Track old child before execution
        old_child = namespace.get("child")

        # Create a custom file-like object that tees output to both stdout and a buffer
        class TeeOutput:
            def __init__(self, original_stdout: Any) -> None:
                self.original = original_stdout
                self.buffer = io.StringIO()

            def write(self, data: str) -> int:
                # Write to original stdout for real-time visibility
                # Use the underlying buffer to bypass Python's buffering
                if hasattr(self.original, "buffer"):
                    self.original.buffer.write(data.encode())
                    self.original.buffer.flush()
                else:
                    self.original.write(data)
                    self.original.flush()
                # Also capture in buffer
                self.buffer.write(data)
                return len(data)

            def flush(self) -> None:
                if hasattr(self.original, "buffer"):
                    self.original.buffer.flush()
                else:
                    self.original.flush()

            def getvalue(self) -> str:
                return self.buffer.getvalue()

        # Replace stdout temporarily with our tee
        tee = TeeOutput(sys.stdout)
        old_stdout = sys.stdout
        sys.stdout = tee

        try:
            # Execute and capture output
            exec(code, namespace)  # noqa: S102
            sys.stdout.flush()
        finally:
            # Restore original stdout
            sys.stdout = old_stdout

        # Cleanup old child if it was replaced
        new_child = namespace.get("child")
        if new_child is not old_child and old_child is not None:
            cleanup_child(old_child)

        result = tee.getvalue()

        # Send response
        response = {
            "status": "ok",
            "output": result,
            "child_alive": bool(namespace["child"] and namespace["child"].isalive()),
        }
        conn.sendall(json.dumps(response).encode())

    except Exception as e:  # noqa: BLE001
        print(f"[{timestamp}] ERROR: {e}", file=sys.stderr)
        traceback.print_exc()
        sys.stderr.flush()

        error_response = {
            "status": "error",
            "error": str(e),
            "traceback": traceback.format_exc(),
        }
        conn.sendall(json.dumps(error_response).encode())

    finally:
        conn.close()


def main() -> None:
    """Run the pexpect server."""
    parser = argparse.ArgumentParser(description="pexpect-server: Session server")
    parser.add_argument("task_id", help="Pueue task ID for this session")
    args = parser.parse_args()

    task_id = args.task_id
    sock_path = get_socket_path(task_id)

    # Create socket
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    with suppress(FileNotFoundError):
        sock_path.unlink()
    sock.bind(str(sock_path))
    sock.listen(1)

    print(f"[pexpect-server] Session {task_id} ready")
    print(f"[pexpect-server] Socket: {sock_path}")
    sys.stdout.flush()

    # Persistent namespace
    namespace: dict[str, Any] = {"pexpect": pexpect, "child": None}

    def signal_handler(signum: int, frame: Any) -> None:
        """Handle shutdown signals."""
        print(f"\n[pexpect-server] Received signal {signum}, shutting down...")
        cleanup_child(namespace.get("child"))
        sys.exit(0)

    # Register signal handlers
    signal.signal(signal.SIGTERM, signal_handler)
    signal.signal(signal.SIGINT, signal_handler)

    try:
        while True:
            conn, _ = sock.accept()
            handle_request(conn, namespace)

    finally:
        # Cleanup child process on shutdown
        cleanup_child(namespace.get("child"))
        sock.close()
        with suppress(FileNotFoundError):
            sock_path.unlink()


if __name__ == "__main__":
    main()
