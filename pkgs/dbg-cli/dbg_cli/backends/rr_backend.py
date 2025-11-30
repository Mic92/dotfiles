"""RR (Record and Replay) debugger backend implementation.

RR is a deterministic replay debugger that uses GDB as its interface.
This backend communicates with RR via the GDB Machine Interface (MI) protocol.
"""

from __future__ import annotations

import os
import re
import shutil
import subprocess
from pathlib import Path
from typing import Any

from dbg_cli.backends.base import DebuggerBackend
from dbg_cli.protocol import (
    Breakpoint,
    DisassemblyLine,
    ErrorType,
    Frame,
    MemoryRegion,
    Register,
    Response,
    SourceLine,
    StopReason,
    Thread,
    Variable,
    error_response,
    ok_response,
    stopped_response,
)
from dbg_cli.state import StateManager


class GDBMIError(Exception):
    """GDB/MI protocol error."""

    pass


class RRBackend(DebuggerBackend):
    """RR replay debugger backend using GDB/MI protocol."""

    def __init__(self, state: StateManager) -> None:
        super().__init__(state)
        self._process: subprocess.Popen[str] | None = None
        self._trace_dir: str | None = None
        self._token = 0

    @property
    def name(self) -> str:
        return "rr"

    @property
    def supports_reverse(self) -> bool:
        return True

    def _next_token(self) -> int:
        """Get next command token."""
        self._token += 1
        return self._token

    def _send_command(self, command: str) -> dict[str, Any]:
        """Send a GDB/MI command and get response."""
        if not self._process:
            raise GDBMIError("GDB process not running")

        token = self._next_token()
        full_command = f"{token}-{command}\n"

        self._process.stdin.write(full_command)
        self._process.stdin.flush()

        return self._read_response(token)

    def _read_response(self, expected_token: int) -> dict[str, Any]:
        """Read GDB/MI response."""
        if not self._process:
            raise GDBMIError("GDB process not running")

        result: dict[str, Any] = {"records": []}

        while True:
            line = self._process.stdout.readline()
            if not line:
                raise GDBMIError("GDB process terminated unexpectedly")

            line = line.strip()
            if not line:
                continue

            # Parse MI output
            parsed = self._parse_mi_output(line)
            if parsed:
                if parsed.get("token") == expected_token:
                    result.update(parsed)
                    if parsed.get("type") in ("result", "error"):
                        break
                else:
                    result["records"].append(parsed)

        return result

    def _parse_mi_output(self, line: str) -> dict[str, Any] | None:
        """Parse a GDB/MI output line."""
        if not line:
            return None

        # Result record: token^result-class,results
        match = re.match(r"^(\d+)\^(\w+)(?:,(.*))?$", line)
        if match:
            token, result_class, results = match.groups()
            return {
                "type": "result" if result_class == "done" else "error",
                "token": int(token),
                "class": result_class,
                "data": self._parse_mi_results(results) if results else {},
            }

        # Async record: *stopped,reason="..."
        match = re.match(r"^\*(\w+)(?:,(.*))?$", line)
        if match:
            async_class, results = match.groups()
            return {
                "type": "async",
                "class": async_class,
                "data": self._parse_mi_results(results) if results else {},
            }

        # Stream records: ~"...", @"...", &"..."
        if line.startswith(("~", "@", "&")):
            return {"type": "stream", "text": line[2:-1] if line.endswith('"') else line[1:]}

        # (gdb) prompt
        if line == "(gdb)":
            return None

        return None

    def _parse_mi_results(self, text: str) -> dict[str, Any]:
        """Parse MI result values (simplified parser)."""
        result: dict[str, Any] = {}
        if not text:
            return result

        # Simple key="value" or key={...} or key=[...] parsing
        # This is a simplified implementation
        i = 0
        while i < len(text):
            # Skip whitespace and commas
            while i < len(text) and text[i] in " ,":
                i += 1
            if i >= len(text):
                break

            # Find key
            key_end = text.find("=", i)
            if key_end == -1:
                break
            key = text[i:key_end]
            i = key_end + 1

            if i >= len(text):
                break

            # Parse value
            if text[i] == '"':
                # String value
                i += 1
                value_end = i
                while value_end < len(text):
                    if text[value_end] == '"' and text[value_end - 1] != "\\":
                        break
                    value_end += 1
                result[key] = text[i:value_end].replace('\\"', '"').replace("\\n", "\n")
                i = value_end + 1
            elif text[i] == "{":
                # Tuple/struct value
                depth = 1
                value_start = i + 1
                i += 1
                while i < len(text) and depth > 0:
                    if text[i] == "{":
                        depth += 1
                    elif text[i] == "}":
                        depth -= 1
                    i += 1
                result[key] = self._parse_mi_results(text[value_start : i - 1])
            elif text[i] == "[":
                # List value
                depth = 1
                value_start = i + 1
                i += 1
                while i < len(text) and depth > 0:
                    if text[i] == "[":
                        depth += 1
                    elif text[i] == "]":
                        depth -= 1
                    i += 1
                result[key] = self._parse_mi_list(text[value_start : i - 1])
            else:
                # Unquoted value
                value_end = i
                while value_end < len(text) and text[value_end] not in ",}]":
                    value_end += 1
                result[key] = text[i:value_end]
                i = value_end

        return result

    def _parse_mi_list(self, text: str) -> list[Any]:
        """Parse MI list values."""
        result: list[Any] = []
        if not text:
            return result

        # Split on commas (simplified - doesn't handle nested structures well)
        i = 0
        while i < len(text):
            while i < len(text) and text[i] in " ,":
                i += 1
            if i >= len(text):
                break

            if text[i] == "{":
                # Tuple element
                depth = 1
                start = i + 1
                i += 1
                while i < len(text) and depth > 0:
                    if text[i] == "{":
                        depth += 1
                    elif text[i] == "}":
                        depth -= 1
                    i += 1
                result.append(self._parse_mi_results(text[start : i - 1]))
            elif text[i] == '"':
                # String element
                i += 1
                end = i
                while end < len(text) and text[end] != '"':
                    if text[end] == "\\":
                        end += 1
                    end += 1
                result.append(text[i:end])
                i = end + 1
            else:
                # Skip unknown
                while i < len(text) and text[i] not in ",}]":
                    i += 1

        return result

    def _start_rr(self, trace_dir: str | None = None) -> None:
        """Start RR replay session."""
        rr_path = shutil.which("rr")
        if not rr_path:
            raise GDBMIError("RR not found in PATH")

        cmd = [rr_path, "replay", "--interpreter=mi"]
        if trace_dir:
            cmd.append(trace_dir)

        self._process = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
        )

        # Read initial output until we get the prompt
        while True:
            line = self._process.stdout.readline()
            if not line or line.strip() == "(gdb)":
                break

    def _stop_reason_from_mi(self, data: dict[str, Any]) -> StopReason:
        """Convert GDB/MI stop reason to our enum."""
        reason = data.get("reason", "")

        if reason == "breakpoint-hit":
            return StopReason.BREAKPOINT
        elif reason == "watchpoint-trigger":
            return StopReason.WATCHPOINT
        elif reason in ("end-stepping-range", "function-finished"):
            return StopReason.STEP
        elif reason == "signal-received":
            return StopReason.SIGNAL
        elif reason == "exited":
            return StopReason.EXITED
        elif reason == "exited-normally":
            return StopReason.EXITED
        else:
            return StopReason.NONE

    def _frame_from_mi(self, data: dict[str, Any], index: int = 0) -> Frame:
        """Convert MI frame data to Frame."""
        return Frame(
            index=index,
            function=data.get("func", "<unknown>"),
            file=data.get("fullname") or data.get("file"),
            line=int(data.get("line", 0)) or None,
            address=data.get("addr", "0x0"),
            module=None,
            args=[],
        )

    def _refresh_state(self) -> None:
        """Refresh state from GDB."""
        if not self._process:
            self.state.clear_process()
            return

        # Get thread info
        try:
            resp = self._send_command("thread-info")
            threads = []
            current_thread_id = None

            if "threads" in resp.get("data", {}):
                for t in resp["data"]["threads"]:
                    thread_id = int(t.get("id", 0))
                    is_current = t.get("current") == "*"
                    if is_current:
                        current_thread_id = thread_id

                    frame_data = t.get("frame", {})
                    frame = self._frame_from_mi(frame_data) if frame_data else None

                    threads.append(Thread(
                        id=thread_id,
                        name=t.get("target-id"),
                        is_current=is_current,
                        stop_reason=StopReason.NONE,
                        frame=frame,
                    ))

            self.state.set_threads(threads)
            self.state.set_current_thread(current_thread_id)

            # Get current frame
            resp = self._send_command("stack-info-frame")
            if "frame" in resp.get("data", {}):
                frame = self._frame_from_mi(resp["data"]["frame"])
                self.state.set_current_frame(frame)

        except GDBMIError:
            pass

        # Get breakpoints
        self._refresh_breakpoints()

    def _refresh_breakpoints(self) -> None:
        """Refresh breakpoint list from GDB."""
        if not self._process:
            self.state.set_breakpoints([])
            return

        try:
            resp = self._send_command("break-list")
            breakpoints = []

            body = resp.get("data", {}).get("BreakpointTable", {}).get("body", [])
            for bp_data in body:
                if isinstance(bp_data, dict):
                    breakpoints.append(Breakpoint(
                        id=int(bp_data.get("number", 0)),
                        location=bp_data.get("original-location", ""),
                        file=bp_data.get("fullname") or bp_data.get("file"),
                        line=int(bp_data.get("line", 0)) or None,
                        address=bp_data.get("addr"),
                        enabled=bp_data.get("enabled") == "y",
                        hit_count=int(bp_data.get("times", 0)),
                        condition=bp_data.get("cond"),
                    ))

            self.state.set_breakpoints(breakpoints)
        except GDBMIError:
            pass

    def _check_running(self) -> Response | None:
        """Check if GDB is running."""
        if not self._process:
            return error_response(
                ErrorType.NOT_RUNNING,
                "No RR session active",
                self.state.get_state(),
            )
        return None

    # Session management

    def launch(self, binary: str, args: list[str]) -> Response:
        """Launch a new RR replay session.

        For RR, 'binary' is the trace directory, not an executable.
        """
        if self._process:
            return error_response(
                ErrorType.ALREADY_RUNNING,
                "RR session already active",
                self.state.get_state(),
            )

        # Check if it's a trace directory
        trace_dir = Path(binary)
        if not trace_dir.exists():
            # Try default trace directory
            default_trace = Path.home() / ".local/share/rr" / binary
            if default_trace.exists():
                trace_dir = default_trace
            else:
                return error_response(
                    ErrorType.TARGET_ERROR,
                    f"Trace directory not found: {binary}",
                    self.state.get_state(),
                )

        try:
            self._start_rr(str(trace_dir))
            self._trace_dir = str(trace_dir)

            # Run to first event
            resp = self._send_command("exec-continue")

            self._refresh_state()
            self.state.set_process(os.getpid(), str(trace_dir))  # RR doesn't give us the original PID easily

            stop_reason = StopReason.NONE
            for record in resp.get("records", []):
                if record.get("class") == "stopped":
                    stop_reason = self._stop_reason_from_mi(record.get("data", {}))

            return stopped_response(
                self.state.get_state(),
                stop_reason,
                {"trace_dir": str(trace_dir)},
            )

        except GDBMIError as e:
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Failed to start RR: {e}",
                self.state.get_state(),
            )

    def attach(self, pid: int) -> Response:
        """RR doesn't support attaching to processes."""
        return error_response(
            ErrorType.DEBUGGER_ERROR,
            "RR does not support attaching. Use 'launch' with a trace directory.",
            self.state.get_state(),
        )

    def detach(self) -> Response:
        """Detach from RR session."""
        return self.quit()

    def quit(self) -> Response:
        """Quit the RR session."""
        if self._process:
            try:
                self._send_command("gdb-exit")
            except GDBMIError:
                pass

            self._process.terminate()
            self._process.wait(timeout=5)
            self._process = None

        self._trace_dir = None
        self.state.reset()

        return ok_response(self.state.get_state())

    # Execution control

    def _exec_and_wait(self, command: str) -> Response:
        """Execute a command and wait for stop."""
        if err := self._check_running():
            return err

        try:
            resp = self._send_command(command)

            if resp.get("class") == "error":
                return error_response(
                    ErrorType.DEBUGGER_ERROR,
                    resp.get("data", {}).get("msg", "Unknown error"),
                    self.state.get_state(),
                )

            self._refresh_state()

            # Find stop reason from async records
            stop_reason = StopReason.STEP
            for record in resp.get("records", []):
                if record.get("class") == "stopped":
                    stop_reason = self._stop_reason_from_mi(record.get("data", {}))
                    if stop_reason == StopReason.EXITED:
                        return stopped_response(
                            self.state.get_state(),
                            StopReason.EXITED,
                            {"exit_status": record.get("data", {}).get("exit-code", 0)},
                        )

            return stopped_response(self.state.get_state(), stop_reason)

        except GDBMIError as e:
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                str(e),
                self.state.get_state(),
            )

    def continue_execution(self) -> Response:
        """Continue execution."""
        return self._exec_and_wait("exec-continue")

    def step(self, count: int = 1) -> Response:
        """Step into."""
        for _ in range(count - 1):
            resp = self._exec_and_wait("exec-step")
            if resp.status == "error":
                return resp
        return self._exec_and_wait("exec-step")

    def next(self, count: int = 1) -> Response:
        """Step over."""
        for _ in range(count - 1):
            resp = self._exec_and_wait("exec-next")
            if resp.status == "error":
                return resp
        return self._exec_and_wait("exec-next")

    def finish(self) -> Response:
        """Run until current function returns."""
        return self._exec_and_wait("exec-finish")

    def reverse_continue(self) -> Response:
        """Reverse continue (RR-specific)."""
        return self._exec_and_wait("exec-continue --reverse")

    def reverse_step(self, count: int = 1) -> Response:
        """Reverse step (RR-specific)."""
        for _ in range(count - 1):
            resp = self._exec_and_wait("exec-step --reverse")
            if resp.status == "error":
                return resp
        return self._exec_and_wait("exec-step --reverse")

    def reverse_next(self, count: int = 1) -> Response:
        """Reverse next (RR-specific)."""
        for _ in range(count - 1):
            resp = self._exec_and_wait("exec-next --reverse")
            if resp.status == "error":
                return resp
        return self._exec_and_wait("exec-next --reverse")

    # Breakpoints

    def breakpoint_set(self, location: str) -> Response:
        """Set a breakpoint."""
        if err := self._check_running():
            return err

        try:
            resp = self._send_command(f"break-insert {location}")

            if resp.get("class") == "error":
                return error_response(
                    ErrorType.DEBUGGER_ERROR,
                    resp.get("data", {}).get("msg", f"Failed to set breakpoint at {location}"),
                    self.state.get_state(),
                )

            bp_data = resp.get("data", {}).get("bkpt", {})
            bp_id = int(bp_data.get("number", 0))

            self._refresh_breakpoints()

            return ok_response(
                self.state.get_state(),
                {"breakpoint_id": bp_id},
            )

        except GDBMIError as e:
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                str(e),
                self.state.get_state(),
            )

    def breakpoint_delete(self, bp_id: int) -> Response:
        """Delete a breakpoint."""
        if err := self._check_running():
            return err

        try:
            resp = self._send_command(f"break-delete {bp_id}")

            if resp.get("class") == "error":
                return error_response(
                    ErrorType.DEBUGGER_ERROR,
                    f"Breakpoint {bp_id} not found",
                    self.state.get_state(),
                )

            self._refresh_breakpoints()
            return ok_response(self.state.get_state())

        except GDBMIError as e:
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                str(e),
                self.state.get_state(),
            )

    def breakpoint_list(self) -> Response:
        """List all breakpoints."""
        self._refresh_breakpoints()
        return ok_response(
            self.state.get_state(),
            {"breakpoints": [bp.to_dict() for bp in self.state.get_state().breakpoints]},
        )

    def breakpoint_enable(self, bp_id: int) -> Response:
        """Enable a breakpoint."""
        if err := self._check_running():
            return err

        try:
            self._send_command(f"break-enable {bp_id}")
            self._refresh_breakpoints()
            return ok_response(self.state.get_state())
        except GDBMIError as e:
            return error_response(ErrorType.DEBUGGER_ERROR, str(e), self.state.get_state())

    def breakpoint_disable(self, bp_id: int) -> Response:
        """Disable a breakpoint."""
        if err := self._check_running():
            return err

        try:
            self._send_command(f"break-disable {bp_id}")
            self._refresh_breakpoints()
            return ok_response(self.state.get_state())
        except GDBMIError as e:
            return error_response(ErrorType.DEBUGGER_ERROR, str(e), self.state.get_state())

    def watchpoint_set(self, expression: str) -> Response:
        """Set a watchpoint."""
        if err := self._check_running():
            return err

        try:
            resp = self._send_command(f"break-watch {expression}")

            if resp.get("class") == "error":
                return error_response(
                    ErrorType.DEBUGGER_ERROR,
                    resp.get("data", {}).get("msg", f"Failed to set watchpoint on {expression}"),
                    self.state.get_state(),
                )

            wp_data = resp.get("data", {}).get("wpt", {})
            wp_id = int(wp_data.get("number", 0))

            return ok_response(
                self.state.get_state(),
                {"watchpoint_id": wp_id},
            )

        except GDBMIError as e:
            return error_response(ErrorType.DEBUGGER_ERROR, str(e), self.state.get_state())

    # Inspection

    def backtrace(self, count: int | None = None) -> Response:
        """Get stack backtrace."""
        if err := self._check_running():
            return err

        try:
            cmd = "stack-list-frames"
            if count is not None:
                cmd += f" 0 {count - 1}"

            resp = self._send_command(cmd)

            frames = []
            for i, f in enumerate(resp.get("data", {}).get("stack", [])):
                if isinstance(f, dict):
                    frames.append(self._frame_from_mi(f, i).to_dict())

            return ok_response(self.state.get_state(), {"frames": frames})

        except GDBMIError as e:
            return error_response(ErrorType.DEBUGGER_ERROR, str(e), self.state.get_state())

    def frame_select(self, index: int) -> Response:
        """Select a stack frame."""
        if err := self._check_running():
            return err

        try:
            self._send_command(f"stack-select-frame {index}")
            self._refresh_state()
            return ok_response(self.state.get_state())
        except GDBMIError as e:
            return error_response(ErrorType.DEBUGGER_ERROR, str(e), self.state.get_state())

    def thread_select(self, thread_id: int) -> Response:
        """Select a thread."""
        if err := self._check_running():
            return err

        try:
            self._send_command(f"thread-select {thread_id}")
            self._refresh_state()
            return ok_response(self.state.get_state())
        except GDBMIError as e:
            return error_response(ErrorType.DEBUGGER_ERROR, str(e), self.state.get_state())

    def thread_list(self) -> Response:
        """List all threads."""
        self._refresh_state()
        return ok_response(
            self.state.get_state(),
            {"threads": [t.to_dict() for t in self.state.get_state().threads]},
        )

    def _variable_from_mi(self, data: dict[str, Any]) -> Variable:
        """Convert MI variable data to Variable."""
        return Variable(
            name=data.get("name", "<unnamed>"),
            value=data.get("value", "<unavailable>"),
            type=data.get("type", "<unknown>"),
            children=[],
        )

    def locals(self) -> Response:
        """Get local variables."""
        if err := self._check_running():
            return err

        try:
            resp = self._send_command("stack-list-locals --all-values")

            variables = []
            for v in resp.get("data", {}).get("locals", []):
                if isinstance(v, dict):
                    variables.append(self._variable_from_mi(v).to_dict())

            return ok_response(self.state.get_state(), {"variables": variables})

        except GDBMIError as e:
            return error_response(ErrorType.DEBUGGER_ERROR, str(e), self.state.get_state())

    def args(self) -> Response:
        """Get function arguments."""
        if err := self._check_running():
            return err

        try:
            resp = self._send_command("stack-list-arguments --all-values 0 0")

            arguments = []
            frames = resp.get("data", {}).get("stack-args", [])
            if frames and isinstance(frames[0], dict):
                for a in frames[0].get("args", []):
                    if isinstance(a, dict):
                        arguments.append(self._variable_from_mi(a).to_dict())

            return ok_response(self.state.get_state(), {"arguments": arguments})

        except GDBMIError as e:
            return error_response(ErrorType.DEBUGGER_ERROR, str(e), self.state.get_state())

    def print_expr(self, expression: str) -> Response:
        """Evaluate and print an expression."""
        if err := self._check_running():
            return err

        try:
            resp = self._send_command(f'data-evaluate-expression "{expression}"')

            if resp.get("class") == "error":
                return error_response(
                    ErrorType.DEBUGGER_ERROR,
                    resp.get("data", {}).get("msg", f"Cannot evaluate: {expression}"),
                    self.state.get_state(),
                )

            value = resp.get("data", {}).get("value", "<unavailable>")

            return ok_response(
                self.state.get_state(),
                {"result": Variable(name=expression, value=value, type="<expr>").to_dict()},
            )

        except GDBMIError as e:
            return error_response(ErrorType.DEBUGGER_ERROR, str(e), self.state.get_state())

    def memory_read(self, address: str, size: int) -> Response:
        """Read memory."""
        if err := self._check_running():
            return err

        try:
            resp = self._send_command(f"data-read-memory-bytes {address} {size}")

            if resp.get("class") == "error":
                return error_response(
                    ErrorType.DEBUGGER_ERROR,
                    resp.get("data", {}).get("msg", f"Cannot read memory at {address}"),
                    self.state.get_state(),
                )

            memory = resp.get("data", {}).get("memory", [{}])[0]
            hex_data = memory.get("contents", "")

            # Convert hex to ASCII
            try:
                data_bytes = bytes.fromhex(hex_data)
                ascii_data = "".join(chr(b) if 32 <= b < 127 else "." for b in data_bytes)
            except ValueError:
                ascii_data = None

            return ok_response(
                self.state.get_state(),
                {
                    "memory": MemoryRegion(
                        address=address,
                        size=size,
                        data=hex_data,
                        ascii=ascii_data,
                    ).to_dict()
                },
            )

        except GDBMIError as e:
            return error_response(ErrorType.DEBUGGER_ERROR, str(e), self.state.get_state())

    def registers(self) -> Response:
        """Get register values."""
        if err := self._check_running():
            return err

        try:
            # Get register names
            names_resp = self._send_command("data-list-register-names")
            names = names_resp.get("data", {}).get("register-names", [])

            # Get register values
            values_resp = self._send_command("data-list-register-values x")
            values = values_resp.get("data", {}).get("register-values", [])

            registers = []
            for v in values:
                if isinstance(v, dict):
                    num = int(v.get("number", -1))
                    if 0 <= num < len(names) and names[num]:
                        registers.append(Register(
                            name=names[num],
                            value=v.get("value", "0"),
                        ).to_dict())

            return ok_response(self.state.get_state(), {"registers": registers})

        except GDBMIError as e:
            return error_response(ErrorType.DEBUGGER_ERROR, str(e), self.state.get_state())

    def source(self, location: str | None = None) -> Response:
        """Get source code around location."""
        self._refresh_state()
        state = self.state.get_state()

        if location:
            if ":" in location:
                parts = location.rsplit(":", 1)
                try:
                    line = int(parts[1])
                    path = Path(parts[0])
                    if path.exists():
                        lines = path.read_text().splitlines()
                        start = max(0, line - 6)
                        end = min(len(lines), line + 5)
                        source_lines = [
                            SourceLine(
                                line_number=i + 1,
                                text=lines[i],
                                is_current=(i + 1 == line),
                            ).to_dict()
                            for i in range(start, end)
                        ]
                        return ok_response(state, {"source": source_lines})
                except (ValueError, OSError):
                    pass

        return ok_response(
            state,
            {"source": [s.to_dict() for s in state.source_context]},
        )

    def disassemble(self, location: str | None = None) -> Response:
        """Disassemble code at location."""
        if err := self._check_running():
            return err

        try:
            if location:
                cmd = f"data-disassemble -s {location} -e {location}+100 -- 0"
            else:
                cmd = "data-disassemble -s $pc -e $pc+100 -- 0"

            resp = self._send_command(cmd)

            disasm = []
            for inst in resp.get("data", {}).get("asm_insns", []):
                if isinstance(inst, dict):
                    disasm.append(DisassemblyLine(
                        address=inst.get("address", "0x0"),
                        opcode=inst.get("inst", "").split()[0] if inst.get("inst") else "",
                        operands=" ".join(inst.get("inst", "").split()[1:]) if inst.get("inst") else "",
                        is_current=False,
                        symbol=inst.get("func-name"),
                    ).to_dict())

            return ok_response(self.state.get_state(), {"disassembly": disasm})

        except GDBMIError as e:
            return error_response(ErrorType.DEBUGGER_ERROR, str(e), self.state.get_state())

    def status(self) -> Response:
        """Get current debugger status."""
        self._refresh_state()
        return ok_response(self.state.get_state())
