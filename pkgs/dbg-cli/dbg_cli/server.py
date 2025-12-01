"""Debugger server - executes Python code with debugger in namespace.

LLMs write Python code to stdin, which is executed with `dbg` (the debugger
session) available in the namespace. Results are returned as JSON.

Similar to pexpect-cli, but for debugger control.
"""

from __future__ import annotations

import argparse
import io
import json
import os
import signal
import socket
import sys
import traceback
from contextlib import suppress
from pathlib import Path
from typing import Any

# Buffer size for socket communication
BUFFER_SIZE = 65536


def get_socket_dir() -> Path:
    """Get the directory for socket files (XDG-compliant)."""
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR")
    if runtime_dir:
        socket_dir = Path(runtime_dir) / "dbg-cli"
    else:
        cache_dir = os.environ.get("XDG_CACHE_HOME", Path.home() / ".cache")
        socket_dir = Path(cache_dir) / "dbg-cli" / "sockets"

    socket_dir.mkdir(parents=True, exist_ok=True, mode=0o700)
    return socket_dir


def get_socket_path(session_id: str) -> Path:
    """Get the socket path for a session."""
    return get_socket_dir() / f"{session_id}.sock"


class TeeOutput:
    """Captures output while also writing to original stream."""

    def __init__(self, original: Any) -> None:
        self.original = original
        self.buffer = io.StringIO()

    def write(self, data: str) -> int:
        self.original.write(data)
        self.buffer.write(data)
        return len(data)

    def flush(self) -> None:
        self.original.flush()

    def getvalue(self) -> str:
        return self.buffer.getvalue()

    def clear(self) -> None:
        self.buffer = io.StringIO()


class DebuggerServer:
    """Server that executes Python code with debugger in namespace."""

    def __init__(self, session_id: str, backend_name: str = "lldb") -> None:
        self.session_id = session_id
        self.socket_path = get_socket_path(session_id)
        self.backend_name = backend_name
        self._running = False
        self._socket: socket.socket | None = None
        self._namespace: dict[str, Any] = {}

    def _setup_namespace(self) -> None:
        """Set up the execution namespace with debugger."""
        from dbg_cli.state import StateManager
        from dbg_cli.protocol import (
            StopReason,
            ErrorType,
            Frame,
            Thread,
            Breakpoint,
            Variable,
            Register,
            MemoryRegion,
            SourceLine,
            DisassemblyLine,
        )

        # Create state manager and backend
        state = StateManager(backend=self.backend_name)

        if self.backend_name == "rr":
            from dbg_cli.backends.rr_backend import RRBackend
            backend = RRBackend(state)
        else:
            from dbg_cli.backends.lldb_backend import LLDBBackend
            backend = LLDBBackend(state)

        # Create the high-level wrapper
        dbg = DebuggerWrapper(backend, state)

        # Create RR utilities (available for trace management)
        rr_utils = RRUtilities()

        # Populate namespace
        self._namespace = {
            # Main debugger interface
            "dbg": dbg,
            # RR trace management (always available)
            "rr": rr_utils,
            # Protocol types for inspection
            "StopReason": StopReason,
            "ErrorType": ErrorType,
            "Frame": Frame,
            "Thread": Thread,
            "Breakpoint": Breakpoint,
            "Variable": Variable,
            "Register": Register,
            "MemoryRegion": MemoryRegion,
            "SourceLine": SourceLine,
            "DisassemblyLine": DisassemblyLine,
            # Useful builtins
            "json": json,
            "print": print,
        }

    def start(self) -> None:
        """Start the server."""
        try:
            self._setup_namespace()
        except Exception as e:
            print(f"Failed to initialize debugger: {e}", file=sys.stderr)
            traceback.print_exc()
            sys.exit(1)

        # Clean up old socket
        if self.socket_path.exists():
            self.socket_path.unlink()

        # Create Unix socket
        self._socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self._socket.bind(str(self.socket_path))
        self._socket.listen(1)

        # Signal handlers
        signal.signal(signal.SIGTERM, self._handle_signal)
        signal.signal(signal.SIGINT, self._handle_signal)

        print(f"dbg-server started: {self.socket_path}", file=sys.stderr)
        print(f"Backend: {self.backend_name}", file=sys.stderr)

        self._running = True
        self._serve()

    def _handle_signal(self, signum: int, frame: object) -> None:
        """Handle termination signals."""
        print(f"Received signal {signum}, shutting down...", file=sys.stderr)
        self._running = False

        # Cleanup debugger
        dbg = self._namespace.get("dbg")
        if dbg:
            with suppress(Exception):
                dbg.quit()

    def _serve(self) -> None:
        """Main server loop."""
        while self._running:
            try:
                self._socket.settimeout(1.0)
                try:
                    conn, _ = self._socket.accept()
                except socket.timeout:
                    continue

                self._handle_connection(conn)

            except Exception as e:
                if self._running:
                    print(f"Server error: {e}", file=sys.stderr)

        self._cleanup()

    def _handle_connection(self, conn: socket.socket) -> None:
        """Handle a single client connection."""
        try:
            # Read code
            data = b""
            while True:
                chunk = conn.recv(BUFFER_SIZE)
                if not chunk:
                    break
                data += chunk
                # Check for complete JSON
                try:
                    json.loads(data.decode())
                    break
                except json.JSONDecodeError:
                    continue

            if not data:
                return

            request = json.loads(data.decode())
            code = request.get("code", "")

            # Execute code
            response = self._execute_code(code)

            # Send response
            conn.sendall(json.dumps(response).encode())

        except json.JSONDecodeError as e:
            error_response = {
                "status": "error",
                "error": f"Invalid JSON: {e}",
                "output": "",
            }
            conn.sendall(json.dumps(error_response).encode())

        except Exception as e:
            error_response = {
                "status": "error",
                "error": str(e),
                "output": "",
            }
            with suppress(Exception):
                conn.sendall(json.dumps(error_response).encode())

        finally:
            conn.close()

    def _execute_code(self, code: str) -> dict[str, Any]:
        """Execute Python code and return result."""
        tee = TeeOutput(sys.stdout)
        result: Any = None
        error: str | None = None
        tb: str | None = None

        try:
            # Capture stdout
            old_stdout = sys.stdout
            sys.stdout = tee

            # Execute code
            # Use exec for statements, eval for expressions
            try:
                # Try as expression first
                result = eval(code, self._namespace)
            except SyntaxError:
                # Fall back to exec for statements
                exec(code, self._namespace)
                result = None

        except Exception as e:
            error = str(e)
            tb = traceback.format_exc()

        finally:
            sys.stdout = old_stdout

        # Get debugger state
        dbg = self._namespace.get("dbg")
        state_dict = None
        if dbg:
            try:
                state_dict = dbg.get_state_dict()
            except Exception:
                pass

        # Build response
        response: dict[str, Any] = {
            "status": "error" if error else "ok",
            "output": tee.getvalue(),
        }

        if result is not None:
            # Try to serialize result
            try:
                if hasattr(result, "to_dict"):
                    response["result"] = result.to_dict()
                else:
                    json.dumps(result)  # Test if serializable
                    response["result"] = result
            except (TypeError, ValueError):
                response["result"] = repr(result)

        if error:
            response["error"] = error
            response["traceback"] = tb

        if state_dict:
            response["state"] = state_dict

        return response

    def _cleanup(self) -> None:
        """Clean up resources."""
        if self._socket:
            with suppress(Exception):
                self._socket.close()

        if self.socket_path.exists():
            with suppress(Exception):
                self.socket_path.unlink()

        print("Server shutdown complete", file=sys.stderr)


class DebuggerWrapper:
    """High-level debugger wrapper for the namespace.

    This provides a clean Python interface that LLMs can use.
    All methods return dicts suitable for JSON serialization.

    Use dbg.help() to discover available methods.
    """

    # Method categories for discoverability
    _CATEGORIES = {
        "discovery": ["help", "methods", "backend_info"],
        "session": ["launch", "attach", "detach", "quit"],
        "execution": ["continue_", "step", "next", "finish"],
        "reverse": ["reverse_continue", "reverse_step", "reverse_next"],
        "breakpoints": ["breakpoint", "delete", "breakpoints", "enable", "disable", "watch"],
        "inspection": ["backtrace", "frame", "thread", "threads", "locals", "args", "print", "memory", "registers"],
        "source": ["source", "disassemble", "status"],
        "rr": ["checkpoint", "restart", "when", "run_to_event", "checkpoints", "delete_checkpoint"],
        "multiprocess": ["targets", "select_target", "add_target", "follow_fork", "async_mode"],
        "advanced": ["inject_library", "find_symbol", "memory_search", "signal_handler", "environment"],
    }

    def __init__(self, backend: Any, state: Any) -> None:
        self._backend = backend
        self._state = state

    # ===== API Discoverability =====

    def help(self, method: str | None = None) -> dict[str, Any]:
        """Get help on available methods.

        Args:
            method: Specific method name, or None for overview

        Examples:
            dbg.help()              # List all categories and methods
            dbg.help("breakpoint")  # Get help on breakpoint method
            dbg.help("execution")   # List methods in execution category
        """
        import inspect

        if method is None:
            # Return overview of all categories
            result = {
                "backend": self._backend.name,
                "categories": {},
                "hint": "Use dbg.help('method_name') for details on a specific method",
            }
            for cat, methods in self._CATEGORIES.items():
                available = []
                for m in methods:
                    if hasattr(self, m):
                        func = getattr(self, m)
                        if callable(func):
                            doc = func.__doc__
                            summary = doc.split("\n")[0] if doc else ""
                            available.append({"name": m, "summary": summary})
                if available:
                    result["categories"][cat] = available
            return result

        # Check if it's a category name
        if method in self._CATEGORIES:
            methods_info = []
            for m in self._CATEGORIES[method]:
                if hasattr(self, m):
                    func = getattr(self, m)
                    if callable(func):
                        sig = str(inspect.signature(func))
                        doc = func.__doc__ or ""
                        methods_info.append({
                            "name": m,
                            "signature": f"{m}{sig}",
                            "doc": doc.strip(),
                        })
            return {"category": method, "methods": methods_info}

        # Get help on specific method
        if hasattr(self, method):
            func = getattr(self, method)
            if callable(func):
                sig = str(inspect.signature(func))
                doc = func.__doc__ or "No documentation available"
                return {
                    "method": method,
                    "signature": f"{method}{sig}",
                    "doc": doc.strip(),
                }

        return {"error": f"Unknown method or category: {method}"}

    def methods(self) -> list[str]:
        """List all available method names.

        Returns simple list for quick reference. Use help() for details.
        """
        all_methods = []
        for methods in self._CATEGORIES.values():
            for m in methods:
                if hasattr(self, m) and callable(getattr(self, m)):
                    all_methods.append(m)
        return sorted(set(all_methods))

    def backend_info(self) -> dict[str, Any]:
        """Get information about the current backend.

        Returns backend name, capabilities, and backend-specific features.
        """
        info = {
            "name": self._backend.name,
            "supports_reverse": self._backend.supports_reverse,
        }

        if self._backend.name == "rr":
            info["rr_features"] = [
                "checkpoint", "restart", "when", "run_to_event",
                "reverse_continue", "reverse_step", "reverse_next"
            ]
            info["note"] = "Use rr.traces() to list available traces"
        elif self._backend.name == "lldb":
            info["lldb_features"] = [
                "inject_library", "targets", "add_target", "select_target",
                "follow_fork", "async_mode"
            ]

        return info

    def get_state_dict(self) -> dict[str, Any]:
        """Get current state as dict."""
        return self._state.get_state().to_dict()

    def _response_to_dict(self, response: Any) -> dict[str, Any]:
        """Convert response to dict."""
        return response.to_dict()

    # ===== Session management =====

    def launch(self, binary: str, args: list[str] | None = None) -> dict[str, Any]:
        """Launch a program for debugging.

        Args:
            binary: Path to executable (or trace dir for RR)
            args: Command line arguments

        Returns:
            Response dict with state and result
        """
        return self._response_to_dict(
            self._backend.launch(binary, args or [])
        )

    def attach(self, pid: int) -> dict[str, Any]:
        """Attach to a running process."""
        return self._response_to_dict(self._backend.attach(pid))

    def detach(self) -> dict[str, Any]:
        """Detach from current process."""
        return self._response_to_dict(self._backend.detach())

    def quit(self) -> dict[str, Any]:
        """Quit the debugger."""
        return self._response_to_dict(self._backend.quit())

    # Execution control

    def continue_(self) -> dict[str, Any]:
        """Continue execution until next stop."""
        return self._response_to_dict(self._backend.continue_execution())

    # Alias without underscore for convenience
    cont = continue_
    c = continue_

    def step(self, count: int = 1) -> dict[str, Any]:
        """Step into (source level)."""
        return self._response_to_dict(self._backend.step(count))

    s = step

    def next(self, count: int = 1) -> dict[str, Any]:
        """Step over (source level)."""
        return self._response_to_dict(self._backend.next(count))

    n = next

    def finish(self) -> dict[str, Any]:
        """Run until current function returns."""
        return self._response_to_dict(self._backend.finish())

    def reverse_continue(self) -> dict[str, Any]:
        """Reverse continue (RR only)."""
        return self._response_to_dict(self._backend.reverse_continue())

    rc = reverse_continue

    def reverse_step(self, count: int = 1) -> dict[str, Any]:
        """Reverse step (RR only)."""
        return self._response_to_dict(self._backend.reverse_step(count))

    rs = reverse_step

    def reverse_next(self, count: int = 1) -> dict[str, Any]:
        """Reverse next (RR only)."""
        return self._response_to_dict(self._backend.reverse_next(count))

    rn = reverse_next

    # Breakpoints

    def breakpoint(self, location: str) -> dict[str, Any]:
        """Set a breakpoint.

        Args:
            location: Where to break (file:line, function, or address)

        Returns:
            Response with breakpoint_id
        """
        return self._response_to_dict(self._backend.breakpoint_set(location))

    b = breakpoint
    bp = breakpoint

    def delete(self, bp_id: int) -> dict[str, Any]:
        """Delete a breakpoint."""
        return self._response_to_dict(self._backend.breakpoint_delete(bp_id))

    def breakpoints(self) -> dict[str, Any]:
        """List all breakpoints."""
        return self._response_to_dict(self._backend.breakpoint_list())

    def enable(self, bp_id: int) -> dict[str, Any]:
        """Enable a breakpoint."""
        return self._response_to_dict(self._backend.breakpoint_enable(bp_id))

    def disable(self, bp_id: int) -> dict[str, Any]:
        """Disable a breakpoint."""
        return self._response_to_dict(self._backend.breakpoint_disable(bp_id))

    def watch(self, expression: str) -> dict[str, Any]:
        """Set a watchpoint on an expression."""
        return self._response_to_dict(self._backend.watchpoint_set(expression))

    # Inspection

    def backtrace(self, count: int | None = None) -> dict[str, Any]:
        """Get stack backtrace."""
        return self._response_to_dict(self._backend.backtrace(count))

    bt = backtrace

    def frame(self, index: int) -> dict[str, Any]:
        """Select a stack frame."""
        return self._response_to_dict(self._backend.frame_select(index))

    f = frame

    def thread(self, thread_id: int) -> dict[str, Any]:
        """Select a thread."""
        return self._response_to_dict(self._backend.thread_select(thread_id))

    def threads(self) -> dict[str, Any]:
        """List all threads."""
        return self._response_to_dict(self._backend.thread_list())

    def locals(self) -> dict[str, Any]:
        """Get local variables."""
        return self._response_to_dict(self._backend.locals())

    lo = locals

    def args(self) -> dict[str, Any]:
        """Get function arguments."""
        return self._response_to_dict(self._backend.args())

    def print(self, expression: str) -> dict[str, Any]:
        """Evaluate and print an expression."""
        return self._response_to_dict(self._backend.print_expr(expression))

    p = print

    def memory(self, address: str, size: int = 64) -> dict[str, Any]:
        """Read memory."""
        return self._response_to_dict(self._backend.memory_read(address, size))

    x = memory

    def registers(self) -> dict[str, Any]:
        """Get register values."""
        return self._response_to_dict(self._backend.registers())

    reg = registers

    # Source and disassembly

    def source(self, location: str | None = None) -> dict[str, Any]:
        """Get source code around location."""
        return self._response_to_dict(self._backend.source(location))

    l = source
    list = source

    def disassemble(self, location: str | None = None) -> dict[str, Any]:
        """Disassemble code at location."""
        return self._response_to_dict(self._backend.disassemble(location))

    dis = disassemble

    # Status

    def status(self) -> dict[str, Any]:
        """Get current debugger status."""
        return self._response_to_dict(self._backend.status())

    # RR-specific methods (only work with RR backend)

    def checkpoint(self) -> dict[str, Any]:
        """Create a checkpoint at current execution point (RR only).

        Returns checkpoint ID that can be used with restart().
        """
        return self._response_to_dict(self._backend.checkpoint())

    def restart(self, checkpoint_id: int) -> dict[str, Any]:
        """Restart from a checkpoint (RR only)."""
        return self._response_to_dict(self._backend.restart(checkpoint_id))

    def when(self) -> dict[str, Any]:
        """Get current RR event number (RR only).

        Events are the fundamental unit of time in RR traces.
        """
        return self._response_to_dict(self._backend.when())

    def run_to_event(self, event: int) -> dict[str, Any]:
        """Run to a specific RR event (RR only)."""
        return self._response_to_dict(self._backend.run_to_event(event))

    def checkpoints(self) -> dict[str, Any]:
        """List all checkpoints (RR only)."""
        return self._response_to_dict(self._backend.list_checkpoints())

    def delete_checkpoint(self, checkpoint_id: int) -> dict[str, Any]:
        """Delete a checkpoint (RR only)."""
        return self._response_to_dict(self._backend.delete_checkpoint(checkpoint_id))

    # Multi-process support (LLDB)

    def targets(self) -> dict[str, Any]:
        """List all debugger targets (LLDB only).

        Returns list of targets with: index, executable, pid, state, is_selected
        """
        return self._response_to_dict(self._backend.list_targets())

    def select_target(self, index: int) -> dict[str, Any]:
        """Select a target by index (LLDB only)."""
        return self._response_to_dict(self._backend.select_target(index))

    def add_target(self, binary: str) -> dict[str, Any]:
        """Add a new target without launching it (LLDB only).

        Useful for setting breakpoints in multiple executables.
        """
        return self._response_to_dict(self._backend.add_target(binary))

    def follow_fork(self, mode: str) -> dict[str, Any]:
        """Set fork following mode (LLDB only).

        Args:
            mode: "parent" or "child"

        Note: LLDB fork following is limited on macOS.
        """
        return self._response_to_dict(self._backend.set_follow_fork_mode(mode))

    def async_mode(self, enabled: bool) -> dict[str, Any]:
        """Enable/disable async mode (LLDB only).

        Async mode is needed for complex multi-process scenarios.
        """
        return self._response_to_dict(self._backend.set_async_mode(enabled))

    # ===== Advanced Debug Utilities =====

    def inject_library(self, library_path: str) -> dict[str, Any]:
        """Inject a shared library into the debugged process (LLDB only).

        The library will be loaded into the process address space.
        Useful for instrumentation, hooking, or adding debugging aids.

        Args:
            library_path: Path to .so (Linux) or .dylib (macOS)

        Example:
            dbg.inject_library("/path/to/libhooks.dylib")
        """
        return self._response_to_dict(self._backend.inject_library(library_path))

    def find_symbol(self, name: str, exact: bool = False) -> dict[str, Any]:
        """Find symbols matching a name pattern.

        Args:
            name: Symbol name or pattern to search for
            exact: If True, match exactly; if False, substring match

        Returns:
            List of matching symbols with addresses and modules
        """
        return self._response_to_dict(self._backend.find_symbol(name, exact))

    def memory_search(
        self,
        pattern: str | bytes,
        start: str | None = None,
        size: int | None = None,
    ) -> dict[str, Any]:
        """Search memory for a byte pattern.

        Args:
            pattern: Hex string ("deadbeef") or bytes to search for
            start: Start address (default: heap start or 0)
            size: Number of bytes to search (default: reasonable limit)

        Returns:
            List of addresses where pattern was found
        """
        return self._response_to_dict(self._backend.memory_search(pattern, start, size))

    def signal_handler(self, signal: str, action: str = "stop") -> dict[str, Any]:
        """Configure how a signal is handled.

        Args:
            signal: Signal name (e.g., "SIGSEGV", "SIGINT") or number
            action: "stop" (break into debugger), "pass" (pass to process),
                   "ignore" (suppress), or "info" (get current setting)

        Example:
            dbg.signal_handler("SIGPIPE", "ignore")
        """
        return self._response_to_dict(self._backend.signal_handler(signal, action))

    def environment(self, action: str = "list", name: str | None = None, value: str | None = None) -> dict[str, Any]:
        """Manage process environment variables.

        Args:
            action: "list", "get", "set", or "unset"
            name: Variable name (required for get/set/unset)
            value: Variable value (required for set)

        Examples:
            dbg.environment()                          # List all
            dbg.environment("get", "PATH")             # Get specific
            dbg.environment("set", "DEBUG", "1")       # Set variable
            dbg.environment("unset", "DEBUG")          # Remove variable
        """
        return self._response_to_dict(self._backend.environment(action, name, value))


class RRUtilities:
    """RR trace management utilities.

    Available as `rr` in the namespace for managing traces outside debugging sessions.
    Use rr.help() to discover available methods.
    """

    def help(self, method: str | None = None) -> dict[str, Any]:
        """Get help on RR trace management methods.

        Args:
            method: Specific method name, or None for overview

        Examples:
            rr.help()           # List all methods
            rr.help("record")   # Get help on record method
        """
        import inspect

        methods = ["traces", "trace_info", "record", "delete", "events"]

        if method is None:
            result = []
            for m in methods:
                func = getattr(self, m)
                doc = func.__doc__
                summary = doc.split("\n")[0] if doc else ""
                result.append({"name": m, "summary": summary})
            return {"methods": result, "hint": "Use rr.help('method_name') for details"}

        if hasattr(self, method) and method in methods:
            func = getattr(self, method)
            sig = str(inspect.signature(func))
            doc = func.__doc__ or "No documentation"
            return {
                "method": method,
                "signature": f"rr.{method}{sig}",
                "doc": doc.strip(),
            }

        return {"error": f"Unknown method: {method}"}

    def traces(self) -> list[dict[str, Any]]:
        """List all available RR traces.

        Returns list of trace info dicts with: name, path, executable, recorded_at, size_bytes
        """
        from dbg_cli.rr import list_traces
        return [t.to_dict() for t in list_traces()]

    def trace_info(self, trace_path: str) -> dict[str, Any] | None:
        """Get information about a specific trace.

        Args:
            trace_path: Path or name of trace directory
        """
        from dbg_cli.rr import get_trace_info
        info = get_trace_info(trace_path)
        return info.to_dict() if info else None

    def record(
        self,
        binary: str,
        args: list[str] | None = None,
        output_name: str | None = None,
        env: dict[str, str] | None = None,
    ) -> dict[str, Any]:
        """Record a new RR trace.

        Args:
            binary: Path to executable to record
            args: Command line arguments
            output_name: Custom name for trace directory
            env: Additional environment variables

        Returns:
            Dict with trace info or error
        """
        from dbg_cli.rr import record_trace
        return record_trace(binary, args, output_name, env)

    def delete(self, trace_path: str) -> dict[str, Any]:
        """Delete an RR trace.

        Args:
            trace_path: Path or name of trace to delete
        """
        from dbg_cli.rr import delete_trace
        return delete_trace(trace_path)

    def events(self, trace_path: str) -> dict[str, Any]:
        """Get list of processes/events in a trace.

        Args:
            trace_path: Path or name of trace
        """
        from dbg_cli.rr import get_trace_events
        return get_trace_events(trace_path)


def main() -> None:
    """Main entry point for dbg-server."""
    parser = argparse.ArgumentParser(
        description="Debugger server - executes Python code with debugger in namespace",
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
