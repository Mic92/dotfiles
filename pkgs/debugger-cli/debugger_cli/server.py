"""Debugger server - long-running process managed by pueue.

The server maintains the debugger state and handles commands via Unix socket.
"""

from __future__ import annotations

import argparse
import json
import os
import signal
import socket
import sys
from contextlib import suppress
from pathlib import Path
from typing import TYPE_CHECKING

from debugger_cli.backends.base import DebuggerBackend
from debugger_cli.commands import (
    CommandParseError,
    CommandType,
    ParsedCommand,
    get_help_text,
    parse_command,
)
from debugger_cli.protocol import (
    ErrorType,
    Request,
    Response,
    error_response,
    ok_response,
)
from debugger_cli.state import StateManager

if TYPE_CHECKING:
    pass

# Buffer size for socket communication
BUFFER_SIZE = 65536


def get_socket_dir() -> Path:
    """Get the directory for socket files (XDG-compliant)."""
    # Prefer XDG_RUNTIME_DIR (usually /run/user/UID, tmpfs)
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR")
    if runtime_dir:
        socket_dir = Path(runtime_dir) / "debugger-cli"
    else:
        # Fall back to cache directory
        cache_dir = os.environ.get("XDG_CACHE_HOME", Path.home() / ".cache")
        socket_dir = Path(cache_dir) / "debugger-cli" / "sockets"

    socket_dir.mkdir(parents=True, exist_ok=True, mode=0o700)
    return socket_dir


def get_socket_path(session_id: str) -> Path:
    """Get the socket path for a session."""
    return get_socket_dir() / f"{session_id}.sock"


class DebuggerServer:
    """Server that manages a debugger backend and handles commands."""

    def __init__(self, session_id: str, backend_name: str = "lldb") -> None:
        self.session_id = session_id
        self.socket_path = get_socket_path(session_id)
        self.state = StateManager(backend=backend_name)
        self.backend: DebuggerBackend | None = None
        self.backend_name = backend_name
        self._running = False
        self._socket: socket.socket | None = None

    def _create_backend(self) -> DebuggerBackend:
        """Create the appropriate backend."""
        if self.backend_name == "rr":
            from debugger_cli.backends.rr_backend import RRBackend

            return RRBackend(self.state)
        else:
            from debugger_cli.backends.lldb_backend import LLDBBackend

            return LLDBBackend(self.state)

    def start(self) -> None:
        """Start the server."""
        # Create backend
        try:
            self.backend = self._create_backend()
        except Exception as e:
            print(f"Failed to create backend: {e}", file=sys.stderr)
            sys.exit(1)

        # Clean up old socket if exists
        if self.socket_path.exists():
            self.socket_path.unlink()

        # Create Unix socket
        self._socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self._socket.bind(str(self.socket_path))
        self._socket.listen(1)

        # Set up signal handlers
        signal.signal(signal.SIGTERM, self._handle_signal)
        signal.signal(signal.SIGINT, self._handle_signal)

        print(f"Debugger server started: {self.socket_path}", file=sys.stderr)

        self._running = True
        self._serve()

    def _handle_signal(self, signum: int, frame: object) -> None:
        """Handle termination signals."""
        print(f"Received signal {signum}, shutting down...", file=sys.stderr)
        self._running = False

        # Clean up backend
        if self.backend:
            with suppress(Exception):
                self.backend.quit()

    def _serve(self) -> None:
        """Main server loop."""
        while self._running:
            try:
                # Accept connection with timeout to allow signal handling
                self._socket.settimeout(1.0)
                try:
                    conn, _ = self._socket.accept()
                except socket.timeout:
                    continue

                self._handle_connection(conn)

            except Exception as e:
                if self._running:
                    print(f"Server error: {e}", file=sys.stderr)

        # Cleanup
        self._cleanup()

    def _handle_connection(self, conn: socket.socket) -> None:
        """Handle a single client connection."""
        try:
            # Read request
            data = b""
            while True:
                chunk = conn.recv(BUFFER_SIZE)
                if not chunk:
                    break
                data += chunk
                # Check for complete JSON (simplified - assumes single object)
                try:
                    json.loads(data.decode())
                    break
                except json.JSONDecodeError:
                    continue

            if not data:
                return

            # Parse and execute command
            request_data = json.loads(data.decode())
            response = self._execute_request(request_data)

            # Send response
            response_json = json.dumps(response.to_dict())
            conn.sendall(response_json.encode())

        except json.JSONDecodeError as e:
            error = error_response(
                ErrorType.PARSE_ERROR,
                f"Invalid JSON: {e}",
                self.state.get_state(),
            )
            conn.sendall(json.dumps(error.to_dict()).encode())

        except Exception as e:
            error = error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Server error: {e}",
                self.state.get_state(),
            )
            with suppress(Exception):
                conn.sendall(json.dumps(error.to_dict()).encode())

        finally:
            conn.close()

    def _execute_request(self, request_data: dict) -> Response:
        """Execute a request and return response."""
        # Parse command from request
        command_str = request_data.get("command", "")
        if not command_str:
            return error_response(
                ErrorType.INVALID_COMMAND,
                "No command provided",
                self.state.get_state(),
            )

        try:
            cmd = parse_command(command_str)
        except CommandParseError as e:
            return error_response(
                ErrorType.PARSE_ERROR,
                e.message,
                self.state.get_state(),
            )

        return self._execute_command(cmd)

    def _execute_command(self, cmd: ParsedCommand) -> Response:
        """Execute a parsed command."""
        if not self.backend:
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                "Backend not initialized",
                self.state.get_state(),
            )

        match cmd.type:
            # Session management
            case CommandType.LAUNCH:
                return self.backend.launch(cmd.args["binary"], cmd.args["args"])
            case CommandType.ATTACH:
                return self.backend.attach(cmd.args["pid"])
            case CommandType.DETACH:
                return self.backend.detach()
            case CommandType.QUIT:
                response = self.backend.quit()
                self._running = False  # Stop server after quit
                return response

            # Execution control
            case CommandType.CONTINUE:
                return self.backend.continue_execution()
            case CommandType.STEP:
                return self.backend.step(cmd.args.get("count", 1))
            case CommandType.NEXT:
                return self.backend.next(cmd.args.get("count", 1))
            case CommandType.FINISH:
                return self.backend.finish()
            case CommandType.REVERSE_CONTINUE:
                return self.backend.reverse_continue()
            case CommandType.REVERSE_STEP:
                return self.backend.reverse_step(cmd.args.get("count", 1))
            case CommandType.REVERSE_NEXT:
                return self.backend.reverse_next(cmd.args.get("count", 1))

            # Breakpoints
            case CommandType.BREAKPOINT_SET:
                return self.backend.breakpoint_set(cmd.args["location"])
            case CommandType.BREAKPOINT_DELETE:
                return self.backend.breakpoint_delete(cmd.args["id"])
            case CommandType.BREAKPOINT_LIST:
                return self.backend.breakpoint_list()
            case CommandType.BREAKPOINT_ENABLE:
                return self.backend.breakpoint_enable(cmd.args["id"])
            case CommandType.BREAKPOINT_DISABLE:
                return self.backend.breakpoint_disable(cmd.args["id"])
            case CommandType.WATCHPOINT_SET:
                return self.backend.watchpoint_set(cmd.args["expression"])

            # Inspection
            case CommandType.BACKTRACE:
                return self.backend.backtrace(cmd.args.get("count"))
            case CommandType.FRAME_SELECT:
                return self.backend.frame_select(cmd.args["index"])
            case CommandType.THREAD_SELECT:
                return self.backend.thread_select(cmd.args["id"])
            case CommandType.THREAD_LIST:
                return self.backend.thread_list()
            case CommandType.LOCALS:
                return self.backend.locals()
            case CommandType.ARGS:
                return self.backend.args()
            case CommandType.PRINT:
                return self.backend.print_expr(cmd.args["expression"])
            case CommandType.MEMORY_READ:
                return self.backend.memory_read(cmd.args["address"], cmd.args["size"])
            case CommandType.REGISTERS:
                return self.backend.registers()

            # Source and disassembly
            case CommandType.SOURCE:
                return self.backend.source(cmd.args.get("location"))
            case CommandType.DISASSEMBLE:
                return self.backend.disassemble(cmd.args.get("location"))

            # Misc
            case CommandType.HELP:
                help_text = get_help_text(cmd.args.get("command"))
                return ok_response(self.state.get_state(), {"help": help_text})
            case CommandType.STATUS:
                return self.backend.status()

            case _:
                return error_response(
                    ErrorType.INVALID_COMMAND,
                    f"Unknown command type: {cmd.type}",
                    self.state.get_state(),
                )

    def _cleanup(self) -> None:
        """Clean up resources."""
        if self._socket:
            with suppress(Exception):
                self._socket.close()

        if self.socket_path.exists():
            with suppress(Exception):
                self.socket_path.unlink()

        print("Server shutdown complete", file=sys.stderr)


def main() -> None:
    """Main entry point for debugger-server."""
    parser = argparse.ArgumentParser(
        description="Debugger server - manages debugger state via Unix socket",
    )
    parser.add_argument(
        "session_id",
        help="Unique session identifier",
    )
    parser.add_argument(
        "--backend",
        "-b",
        choices=["lldb", "rr"],
        default="lldb",
        help="Debugger backend to use (default: lldb)",
    )

    args = parser.parse_args()

    server = DebuggerServer(args.session_id, args.backend)
    server.start()


if __name__ == "__main__":
    main()
