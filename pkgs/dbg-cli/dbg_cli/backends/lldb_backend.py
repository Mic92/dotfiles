"""LLDB debugger backend implementation."""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path
from typing import TYPE_CHECKING, Any

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

if TYPE_CHECKING:
    import lldb


class LLDBLoadError(Exception):
    """Failed to load LLDB Python module."""

    pass


def _find_lldb_module() -> str | None:
    """Find the LLDB Python module path."""
    # Try lldb -P first
    try:
        result = subprocess.run(
            ["lldb", "-P"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if result.returncode == 0:
            path = result.stdout.strip()
            if path and Path(path).exists():
                return path
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass

    # Common paths on Linux
    common_paths = [
        "/usr/lib/python3/dist-packages",
        "/usr/lib64/python3/site-packages",
        "/usr/local/lib/python3/dist-packages",
    ]

    for path in common_paths:
        if Path(path).exists():
            lldb_path = Path(path) / "lldb"
            if lldb_path.exists():
                return path

    return None


def load_lldb() -> Any:
    """Load and return the LLDB module."""
    # Check if already loaded
    if "lldb" in sys.modules:
        return sys.modules["lldb"]

    # Find and add to path
    lldb_path = _find_lldb_module()
    if lldb_path and lldb_path not in sys.path:
        sys.path.insert(0, lldb_path)

    try:
        import lldb

        return lldb
    except ImportError as e:
        raise LLDBLoadError(
            f"Failed to load LLDB Python module: {e}\n"
            "Ensure LLDB is installed and its Python bindings are available."
        ) from e


class LLDBBackend(DebuggerBackend):
    """LLDB debugger backend."""

    def __init__(self, state: StateManager) -> None:
        super().__init__(state)
        self._lldb = load_lldb()
        self._debugger: lldb.SBDebugger = self._lldb.SBDebugger.Create()
        self._debugger.SetAsync(False)  # Synchronous mode for simplicity
        self._target: lldb.SBTarget | None = None
        self._process: lldb.SBProcess | None = None

    @property
    def name(self) -> str:
        return "lldb"

    @property
    def supports_reverse(self) -> bool:
        return False

    def _get_stop_reason(self, thread: lldb.SBThread) -> StopReason:
        """Convert LLDB stop reason to our enum."""
        reason = thread.GetStopReason()
        lldb = self._lldb

        match reason:
            case lldb.eStopReasonBreakpoint:
                return StopReason.BREAKPOINT
            case lldb.eStopReasonWatchpoint:
                return StopReason.WATCHPOINT
            case lldb.eStopReasonTrace | lldb.eStopReasonPlanComplete:
                return StopReason.STEP
            case lldb.eStopReasonSignal:
                return StopReason.SIGNAL
            case lldb.eStopReasonException:
                return StopReason.EXCEPTION
            case lldb.eStopReasonExec:
                return StopReason.NONE
            case _:
                return StopReason.NONE

    def _frame_to_protocol(self, frame: lldb.SBFrame, index: int) -> Frame:
        """Convert LLDB frame to protocol Frame."""
        line_entry = frame.GetLineEntry()
        file_spec = line_entry.GetFileSpec()

        file_path = None
        if file_spec.IsValid():
            file_path = str(file_spec.fullpath) if file_spec.fullpath else None

        # Get function arguments
        args = []
        for i in range(frame.GetFunction().GetArgumentCount() if frame.GetFunction().IsValid() else 0):
            arg = frame.FindVariable(frame.GetFunction().GetArgumentAtIndex(i).GetName())
            if arg.IsValid():
                args.append({"name": arg.GetName(), "value": arg.GetValue() or "?"})

        return Frame(
            index=index,
            function=frame.GetFunctionName() or "<unknown>",
            file=file_path,
            line=line_entry.GetLine() if line_entry.IsValid() else None,
            address=f"0x{frame.GetPC():x}",
            module=frame.GetModule().GetFileSpec().GetFilename() if frame.GetModule().IsValid() else None,
            args=args,
        )

    def _thread_to_protocol(self, thread: lldb.SBThread) -> Thread:
        """Convert LLDB thread to protocol Thread."""
        frame = thread.GetFrameAtIndex(0) if thread.GetNumFrames() > 0 else None
        return Thread(
            id=thread.GetThreadID(),
            name=thread.GetName(),
            is_current=thread.GetThreadID() == self._process.GetSelectedThread().GetThreadID() if self._process else False,
            stop_reason=self._get_stop_reason(thread),
            frame=self._frame_to_protocol(frame, 0) if frame else None,
        )

    def _update_threads(self) -> list[Thread]:
        """Update thread list from LLDB."""
        if not self._process:
            return []

        threads = []
        for i in range(self._process.GetNumThreads()):
            thread = self._process.GetThreadAtIndex(i)
            if thread.IsValid():
                threads.append(self._thread_to_protocol(thread))

        return threads

    def _get_current_frame(self) -> Frame | None:
        """Get current frame from LLDB."""
        if not self._process:
            return None

        thread = self._process.GetSelectedThread()
        if not thread.IsValid():
            return None

        frame = thread.GetSelectedFrame()
        if not frame.IsValid():
            return None

        return self._frame_to_protocol(frame, frame.GetFrameID())

    def _refresh_state(self) -> None:
        """Refresh state from LLDB."""
        if self._process and self._process.IsValid():
            self.state.set_process(self._process.GetProcessID(), self._target.GetExecutable().GetFilename() if self._target else None)
            self.state.set_running(self._process.GetState() == self._lldb.eStateRunning)
        else:
            self.state.clear_process()

        threads = self._update_threads()
        self.state.set_threads(threads)

        if self._process:
            thread = self._process.GetSelectedThread()
            if thread.IsValid():
                self.state.set_current_thread(thread.GetThreadID())

        frame = self._get_current_frame()
        self.state.set_current_frame(frame)

        # Update breakpoints
        self._refresh_breakpoints()

    def _refresh_breakpoints(self) -> None:
        """Refresh breakpoint list from LLDB."""
        if not self._target:
            self.state.set_breakpoints([])
            return

        breakpoints = []
        for i in range(self._target.GetNumBreakpoints()):
            bp = self._target.GetBreakpointAtIndex(i)
            if bp.IsValid():
                # Get first location for file/line info
                loc = bp.GetLocationAtIndex(0) if bp.GetNumLocations() > 0 else None
                addr = loc.GetAddress() if loc else None
                line_entry = addr.GetLineEntry() if addr else None
                file_spec = line_entry.GetFileSpec() if line_entry else None

                breakpoints.append(Breakpoint(
                    id=bp.GetID(),
                    location=str(bp),
                    file=str(file_spec.fullpath) if file_spec and file_spec.IsValid() else None,
                    line=line_entry.GetLine() if line_entry and line_entry.IsValid() else None,
                    address=f"0x{addr.GetLoadAddress(self._target):x}" if addr else None,
                    enabled=bp.IsEnabled(),
                    hit_count=bp.GetHitCount(),
                    condition=bp.GetCondition() if bp.GetCondition() else None,
                ))

        self.state.set_breakpoints(breakpoints)

    def _check_process(self) -> Response | None:
        """Check if we have a valid process. Returns error response if not."""
        if not self._process or not self._process.IsValid():
            return error_response(
                ErrorType.NOT_RUNNING,
                "No process is running",
                self.state.get_state(),
            )
        return None

    def _check_target(self) -> Response | None:
        """Check if we have a valid target. Returns error response if not."""
        if not self._target or not self._target.IsValid():
            return error_response(
                ErrorType.NOT_RUNNING,
                "No target loaded",
                self.state.get_state(),
            )
        return None

    # Session management

    def launch(self, binary: str, args: list[str]) -> Response:
        """Launch a new process."""
        # Create target
        error = self._lldb.SBError()
        self._target = self._debugger.CreateTarget(binary, None, None, True, error)

        if not self._target.IsValid():
            return error_response(
                ErrorType.TARGET_ERROR,
                f"Failed to create target: {error.GetCString()}",
                self.state.get_state(),
            )

        # Launch process
        launch_info = self._target.GetLaunchInfo()
        launch_info.SetArguments(args, True)

        self._process = self._target.Launch(launch_info, error)

        if not self._process or not self._process.IsValid():
            return error_response(
                ErrorType.TARGET_ERROR,
                f"Failed to launch process: {error.GetCString()}",
                self.state.get_state(),
            )

        self._refresh_state()

        stop_reason = StopReason.NONE
        thread = self._process.GetSelectedThread()
        if thread.IsValid():
            stop_reason = self._get_stop_reason(thread)

        return stopped_response(
            self.state.get_state(),
            stop_reason,
            {"pid": self._process.GetProcessID()},
        )

    def attach(self, pid: int) -> Response:
        """Attach to a running process."""
        error = self._lldb.SBError()

        # Create target if needed
        if not self._target:
            self._target = self._debugger.CreateTarget("")

        attach_info = self._lldb.SBAttachInfo(pid)
        self._process = self._target.Attach(attach_info, error)

        if not self._process or not self._process.IsValid():
            return error_response(
                ErrorType.TARGET_ERROR,
                f"Failed to attach to process {pid}: {error.GetCString()}",
                self.state.get_state(),
            )

        self._refresh_state()

        return stopped_response(
            self.state.get_state(),
            StopReason.NONE,
            {"pid": pid},
        )

    def detach(self) -> Response:
        """Detach from current process."""
        if err := self._check_process():
            return err

        error = self._lldb.SBError()
        self._process.Detach(error)

        if error.Fail():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Failed to detach: {error.GetCString()}",
                self.state.get_state(),
            )

        self._process = None
        self.state.clear_process()

        return ok_response(self.state.get_state())

    def quit(self) -> Response:
        """Quit the debugger."""
        if self._process and self._process.IsValid():
            self._process.Kill()
            self._process = None

        if self._target:
            self._debugger.DeleteTarget(self._target)
            self._target = None

        self.state.reset()

        return ok_response(self.state.get_state())

    # Execution control

    def continue_execution(self) -> Response:
        """Continue execution."""
        if err := self._check_process():
            return err

        error = self._lldb.SBError()
        self._process.Continue()

        self._refresh_state()

        # Check if process exited
        if self._process.GetState() == self._lldb.eStateExited:
            exit_status = self._process.GetExitStatus()
            return stopped_response(
                self.state.get_state(),
                StopReason.EXITED,
                {"exit_status": exit_status},
            )

        thread = self._process.GetSelectedThread()
        stop_reason = self._get_stop_reason(thread) if thread.IsValid() else StopReason.NONE

        return stopped_response(self.state.get_state(), stop_reason)

    def step(self, count: int = 1) -> Response:
        """Step into."""
        if err := self._check_process():
            return err

        thread = self._process.GetSelectedThread()
        if not thread.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                "No valid thread selected",
                self.state.get_state(),
            )

        for _ in range(count):
            thread.StepInto()

        self._refresh_state()

        return stopped_response(self.state.get_state(), StopReason.STEP)

    def next(self, count: int = 1) -> Response:
        """Step over."""
        if err := self._check_process():
            return err

        thread = self._process.GetSelectedThread()
        if not thread.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                "No valid thread selected",
                self.state.get_state(),
            )

        for _ in range(count):
            thread.StepOver()

        self._refresh_state()

        return stopped_response(self.state.get_state(), StopReason.STEP)

    def finish(self) -> Response:
        """Run until current function returns."""
        if err := self._check_process():
            return err

        thread = self._process.GetSelectedThread()
        if not thread.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                "No valid thread selected",
                self.state.get_state(),
            )

        thread.StepOut()

        self._refresh_state()

        return stopped_response(self.state.get_state(), StopReason.STEP)

    # Breakpoints

    def breakpoint_set(self, location: str) -> Response:
        """Set a breakpoint."""
        if err := self._check_target():
            return err

        bp = None

        # Try to parse as file:line
        if ":" in location:
            parts = location.rsplit(":", 1)
            try:
                line = int(parts[1])
                bp = self._target.BreakpointCreateByLocation(parts[0], line)
            except ValueError:
                pass

        # Try as function name
        if not bp or not bp.IsValid():
            bp = self._target.BreakpointCreateByName(location)

        # Try as address
        if not bp or not bp.IsValid():
            try:
                addr = int(location, 16) if location.startswith("0x") else int(location)
                bp = self._target.BreakpointCreateByAddress(addr)
            except ValueError:
                pass

        if not bp or not bp.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Failed to set breakpoint at '{location}'",
                self.state.get_state(),
            )

        self._refresh_breakpoints()

        return ok_response(
            self.state.get_state(),
            {"breakpoint_id": bp.GetID()},
        )

    def breakpoint_delete(self, bp_id: int) -> Response:
        """Delete a breakpoint."""
        if err := self._check_target():
            return err

        if not self._target.BreakpointDelete(bp_id):
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Breakpoint {bp_id} not found",
                self.state.get_state(),
            )

        self._refresh_breakpoints()

        return ok_response(self.state.get_state())

    def breakpoint_list(self) -> Response:
        """List all breakpoints."""
        self._refresh_breakpoints()
        return ok_response(
            self.state.get_state(),
            {"breakpoints": [bp.to_dict() for bp in self.state.get_state().breakpoints]},
        )

    def breakpoint_enable(self, bp_id: int) -> Response:
        """Enable a breakpoint."""
        if err := self._check_target():
            return err

        bp = self._target.FindBreakpointByID(bp_id)
        if not bp.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Breakpoint {bp_id} not found",
                self.state.get_state(),
            )

        bp.SetEnabled(True)
        self._refresh_breakpoints()

        return ok_response(self.state.get_state())

    def breakpoint_disable(self, bp_id: int) -> Response:
        """Disable a breakpoint."""
        if err := self._check_target():
            return err

        bp = self._target.FindBreakpointByID(bp_id)
        if not bp.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Breakpoint {bp_id} not found",
                self.state.get_state(),
            )

        bp.SetEnabled(False)
        self._refresh_breakpoints()

        return ok_response(self.state.get_state())

    def watchpoint_set(self, expression: str) -> Response:
        """Set a watchpoint."""
        if err := self._check_process():
            return err

        thread = self._process.GetSelectedThread()
        frame = thread.GetSelectedFrame() if thread.IsValid() else None

        if not frame or not frame.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                "No valid frame for watchpoint",
                self.state.get_state(),
            )

        # Evaluate expression to get address
        value = frame.EvaluateExpression(f"&({expression})")
        if not value.IsValid() or value.GetError().Fail():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Cannot evaluate watchpoint expression: {expression}",
                self.state.get_state(),
            )

        addr = value.GetValueAsUnsigned()
        size = frame.EvaluateExpression(f"sizeof({expression})").GetValueAsUnsigned() or 4

        error = self._lldb.SBError()
        wp = self._target.WatchAddress(addr, size, False, True, error)

        if not wp.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Failed to set watchpoint: {error.GetCString()}",
                self.state.get_state(),
            )

        return ok_response(
            self.state.get_state(),
            {"watchpoint_id": wp.GetID(), "address": f"0x{addr:x}", "size": size},
        )

    # Inspection

    def backtrace(self, count: int | None = None) -> Response:
        """Get stack backtrace."""
        if err := self._check_process():
            return err

        thread = self._process.GetSelectedThread()
        if not thread.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                "No valid thread selected",
                self.state.get_state(),
            )

        frames = []
        num_frames = thread.GetNumFrames()
        if count is not None:
            num_frames = min(num_frames, count)

        for i in range(num_frames):
            frame = thread.GetFrameAtIndex(i)
            if frame.IsValid():
                frames.append(self._frame_to_protocol(frame, i).to_dict())

        return ok_response(self.state.get_state(), {"frames": frames})

    def frame_select(self, index: int) -> Response:
        """Select a stack frame."""
        if err := self._check_process():
            return err

        thread = self._process.GetSelectedThread()
        if not thread.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                "No valid thread selected",
                self.state.get_state(),
            )

        if index < 0 or index >= thread.GetNumFrames():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Invalid frame index: {index}",
                self.state.get_state(),
            )

        thread.SetSelectedFrame(index)
        self._refresh_state()

        return ok_response(self.state.get_state())

    def thread_select(self, thread_id: int) -> Response:
        """Select a thread."""
        if err := self._check_process():
            return err

        for i in range(self._process.GetNumThreads()):
            thread = self._process.GetThreadAtIndex(i)
            if thread.GetThreadID() == thread_id:
                self._process.SetSelectedThread(thread)
                self._refresh_state()
                return ok_response(self.state.get_state())

        return error_response(
            ErrorType.DEBUGGER_ERROR,
            f"Thread {thread_id} not found",
            self.state.get_state(),
        )

    def thread_list(self) -> Response:
        """List all threads."""
        self._refresh_state()
        return ok_response(
            self.state.get_state(),
            {"threads": [t.to_dict() for t in self.state.get_state().threads]},
        )

    def _variable_to_protocol(self, var: lldb.SBValue, max_depth: int = 2) -> Variable:
        """Convert LLDB value to protocol Variable."""
        children = []
        if max_depth > 0 and var.GetNumChildren() > 0:
            for i in range(min(var.GetNumChildren(), 10)):  # Limit children
                child = var.GetChildAtIndex(i)
                if child.IsValid():
                    children.append(self._variable_to_protocol(child, max_depth - 1))

        return Variable(
            name=var.GetName() or "<unnamed>",
            value=var.GetValue() or var.GetSummary() or "<unavailable>",
            type=var.GetTypeName() or "<unknown>",
            children=children,
        )

    def locals(self) -> Response:
        """Get local variables."""
        if err := self._check_process():
            return err

        thread = self._process.GetSelectedThread()
        frame = thread.GetSelectedFrame() if thread.IsValid() else None

        if not frame or not frame.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                "No valid frame",
                self.state.get_state(),
            )

        variables = []
        for var in frame.GetVariables(False, True, False, True):  # locals only
            if var.IsValid():
                variables.append(self._variable_to_protocol(var).to_dict())

        return ok_response(self.state.get_state(), {"variables": variables})

    def args(self) -> Response:
        """Get function arguments."""
        if err := self._check_process():
            return err

        thread = self._process.GetSelectedThread()
        frame = thread.GetSelectedFrame() if thread.IsValid() else None

        if not frame or not frame.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                "No valid frame",
                self.state.get_state(),
            )

        arguments = []
        for var in frame.GetVariables(True, False, False, True):  # args only
            if var.IsValid():
                arguments.append(self._variable_to_protocol(var).to_dict())

        return ok_response(self.state.get_state(), {"arguments": arguments})

    def print_expr(self, expression: str) -> Response:
        """Evaluate and print an expression."""
        if err := self._check_process():
            return err

        thread = self._process.GetSelectedThread()
        frame = thread.GetSelectedFrame() if thread.IsValid() else None

        if not frame or not frame.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                "No valid frame",
                self.state.get_state(),
            )

        result = frame.EvaluateExpression(expression)

        if result.GetError().Fail():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Expression error: {result.GetError().GetCString()}",
                self.state.get_state(),
            )

        return ok_response(
            self.state.get_state(),
            {"result": self._variable_to_protocol(result).to_dict()},
        )

    def memory_read(self, address: str, size: int) -> Response:
        """Read memory."""
        if err := self._check_process():
            return err

        # Parse address
        try:
            if address.startswith("0x"):
                addr = int(address, 16)
            else:
                # Try evaluating as expression
                thread = self._process.GetSelectedThread()
                frame = thread.GetSelectedFrame() if thread.IsValid() else None
                if frame:
                    result = frame.EvaluateExpression(address)
                    addr = result.GetValueAsUnsigned()
                else:
                    addr = int(address)
        except (ValueError, TypeError):
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Invalid address: {address}",
                self.state.get_state(),
            )

        error = self._lldb.SBError()
        data = self._process.ReadMemory(addr, size, error)

        if error.Fail():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Memory read failed: {error.GetCString()}",
                self.state.get_state(),
            )

        # Format as hex and ASCII
        hex_data = data.hex() if data else ""
        ascii_data = "".join(chr(b) if 32 <= b < 127 else "." for b in (data or b""))

        return ok_response(
            self.state.get_state(),
            {
                "memory": MemoryRegion(
                    address=f"0x{addr:x}",
                    size=size,
                    data=hex_data,
                    ascii=ascii_data,
                ).to_dict()
            },
        )

    def registers(self) -> Response:
        """Get register values."""
        if err := self._check_process():
            return err

        thread = self._process.GetSelectedThread()
        frame = thread.GetSelectedFrame() if thread.IsValid() else None

        if not frame or not frame.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                "No valid frame",
                self.state.get_state(),
            )

        registers = []
        for reg_set in frame.GetRegisters():
            for reg in reg_set:
                if reg.IsValid():
                    registers.append(Register(
                        name=reg.GetName(),
                        value=reg.GetValue() or "0",
                    ).to_dict())

        return ok_response(self.state.get_state(), {"registers": registers})

    def source(self, location: str | None = None) -> Response:
        """Get source code around location."""
        self._refresh_state()
        state = self.state.get_state()

        if location:
            # Parse location
            if ":" in location:
                parts = location.rsplit(":", 1)
                try:
                    line = int(parts[1])
                    # Read source file
                    from pathlib import Path
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

        # Return current source context
        return ok_response(
            state,
            {"source": [s.to_dict() for s in state.source_context]},
        )

    def disassemble(self, location: str | None = None) -> Response:
        """Disassemble code at location."""
        if err := self._check_process():
            return err

        thread = self._process.GetSelectedThread()
        frame = thread.GetSelectedFrame() if thread.IsValid() else None

        if not frame or not frame.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                "No valid frame",
                self.state.get_state(),
            )

        # Get function or address to disassemble
        if location:
            # Try as function name
            symbol = self._target.FindFirstSymbol(location)
            if symbol.IsValid():
                instructions = symbol.GetInstructions(self._target)
            else:
                # Try as address
                try:
                    addr = int(location, 16) if location.startswith("0x") else int(location)
                    instructions = self._target.ReadInstructions(
                        self._lldb.SBAddress(addr, self._target), 20
                    )
                except ValueError:
                    return error_response(
                        ErrorType.DEBUGGER_ERROR,
                        f"Invalid location: {location}",
                        self.state.get_state(),
                    )
        else:
            # Disassemble current function
            func = frame.GetFunction()
            if func.IsValid():
                instructions = func.GetInstructions(self._target)
            else:
                # Fall back to current address
                instructions = self._target.ReadInstructions(frame.GetPCAddress(), 20)

        disasm = []
        current_pc = frame.GetPC()
        for inst in instructions:
            disasm.append(DisassemblyLine(
                address=f"0x{inst.GetAddress().GetLoadAddress(self._target):x}",
                opcode=inst.GetMnemonic(self._target),
                operands=inst.GetOperands(self._target),
                is_current=(inst.GetAddress().GetLoadAddress(self._target) == current_pc),
                symbol=inst.GetComment(self._target) if inst.GetComment(self._target) else None,
            ).to_dict())

        return ok_response(self.state.get_state(), {"disassembly": disasm})

    def status(self) -> Response:
        """Get current debugger status."""
        self._refresh_state()
        return ok_response(self.state.get_state())

    # Multi-process support

    def set_follow_fork_mode(self, mode: str) -> Response:
        """Set fork following mode.

        Args:
            mode: "parent" to stay with parent, "child" to follow child process

        Note: LLDB's fork following is limited. On macOS, consider using
        DYLD interposition for better multi-process debugging.
        """
        if mode not in ("parent", "child"):
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Invalid mode: {mode}. Use 'parent' or 'child'",
                self.state.get_state(),
            )

        # LLDB uses settings for fork following
        # Note: This is best-effort as LLDB's fork support is limited
        self._debugger.HandleCommand(f"settings set target.process.follow-fork-mode {mode}")

        return ok_response(
            self.state.get_state(),
            {"follow_fork_mode": mode},
        )

    def list_targets(self) -> Response:
        """List all debugger targets.

        Multiple targets can be loaded for multi-process debugging scenarios.
        """
        targets = []
        for i in range(self._debugger.GetNumTargets()):
            target = self._debugger.GetTargetAtIndex(i)
            if target.IsValid():
                exe = target.GetExecutable()
                process = target.GetProcess()
                targets.append({
                    "index": i,
                    "executable": exe.GetFilename() if exe.IsValid() else None,
                    "path": str(exe.fullpath) if exe.IsValid() and exe.fullpath else None,
                    "pid": process.GetProcessID() if process and process.IsValid() else None,
                    "state": self._process_state_name(process) if process else "no process",
                    "is_selected": target == self._target,
                })

        return ok_response(self.state.get_state(), {"targets": targets})

    def _process_state_name(self, process: Any) -> str:
        """Get human-readable process state name."""
        if not process or not process.IsValid():
            return "invalid"

        lldb = self._lldb
        state = process.GetState()

        state_names = {
            lldb.eStateInvalid: "invalid",
            lldb.eStateUnloaded: "unloaded",
            lldb.eStateConnected: "connected",
            lldb.eStateAttaching: "attaching",
            lldb.eStateLaunching: "launching",
            lldb.eStateStopped: "stopped",
            lldb.eStateRunning: "running",
            lldb.eStateStepping: "stepping",
            lldb.eStateCrashed: "crashed",
            lldb.eStateDetached: "detached",
            lldb.eStateExited: "exited",
            lldb.eStateSuspended: "suspended",
        }

        return state_names.get(state, f"unknown({state})")

    def select_target(self, index: int) -> Response:
        """Select a target by index.

        Args:
            index: Target index from list_targets()
        """
        if index < 0 or index >= self._debugger.GetNumTargets():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Invalid target index: {index}",
                self.state.get_state(),
            )

        target = self._debugger.GetTargetAtIndex(index)
        if not target.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Target at index {index} is not valid",
                self.state.get_state(),
            )

        self._debugger.SetSelectedTarget(target)
        self._target = target
        self._process = target.GetProcess() if target.GetProcess().IsValid() else None

        self._refresh_state()

        return ok_response(
            self.state.get_state(),
            {"selected_target": index},
        )

    def add_target(self, binary: str) -> Response:
        """Add a new target without launching it.

        Useful for multi-process debugging where you want to set breakpoints
        in multiple executables before launching.

        Args:
            binary: Path to executable
        """
        error = self._lldb.SBError()
        target = self._debugger.CreateTarget(binary, None, None, True, error)

        if not target.IsValid():
            return error_response(
                ErrorType.TARGET_ERROR,
                f"Failed to create target: {error.GetCString()}",
                self.state.get_state(),
            )

        index = self._debugger.GetNumTargets() - 1

        return ok_response(
            self.state.get_state(),
            {"target_index": index, "executable": binary},
        )

    def set_async_mode(self, enabled: bool) -> Response:
        """Enable or disable async mode.

        In async mode, execution commands return immediately and you need
        to poll for state changes. Required for complex multi-process scenarios.

        Args:
            enabled: True for async mode, False for sync mode
        """
        self._debugger.SetAsync(enabled)

        return ok_response(
            self.state.get_state(),
            {"async_mode": enabled},
        )

    # Advanced debug utilities

    def inject_library(self, library_path: str) -> Response:
        """Inject a shared library into the process.

        Uses LLDB's expression evaluation to call dlopen().
        """
        if err := self._check_process():
            return err

        thread = self._process.GetSelectedThread()
        frame = thread.GetSelectedFrame() if thread.IsValid() else None

        if not frame or not frame.IsValid():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                "No valid frame for library injection",
                self.state.get_state(),
            )

        # Use dlopen to load the library
        # RTLD_NOW = 2, RTLD_GLOBAL = 0x100 (platform-dependent)
        expr = f'(void*)dlopen("{library_path}", 2)'
        result = frame.EvaluateExpression(expr)

        if result.GetError().Fail():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Failed to inject library: {result.GetError().GetCString()}",
                self.state.get_state(),
            )

        handle = result.GetValueAsUnsigned()
        if handle == 0:
            # Try to get dlerror
            err_result = frame.EvaluateExpression("(char*)dlerror()")
            err_msg = err_result.GetSummary() or "Unknown error"
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"dlopen failed: {err_msg}",
                self.state.get_state(),
            )

        return ok_response(
            self.state.get_state(),
            {"library": library_path, "handle": f"0x{handle:x}"},
        )

    def find_symbol(self, name: str, exact: bool = False) -> Response:
        """Find symbols matching a name pattern."""
        if err := self._check_target():
            return err

        symbols = []
        for module in self._target.module_iter():
            if exact:
                sym = module.FindSymbol(name)
                if sym.IsValid():
                    symbols.append({
                        "name": sym.GetName(),
                        "address": f"0x{sym.GetStartAddress().GetLoadAddress(self._target):x}",
                        "module": module.GetFileSpec().GetFilename(),
                        "type": str(sym.GetType()),
                    })
            else:
                # Iterate through all symbols for substring match
                for sym in module:
                    if name.lower() in (sym.GetName() or "").lower():
                        symbols.append({
                            "name": sym.GetName(),
                            "address": f"0x{sym.GetStartAddress().GetLoadAddress(self._target):x}",
                            "module": module.GetFileSpec().GetFilename(),
                            "type": str(sym.GetType()),
                        })
                        if len(symbols) >= 100:  # Limit results
                            break
            if len(symbols) >= 100:
                break

        return ok_response(
            self.state.get_state(),
            {"symbols": symbols, "count": len(symbols)},
        )

    def memory_search(self, pattern: str | bytes, start: str | None = None, size: int | None = None) -> Response:
        """Search memory for a byte pattern."""
        if err := self._check_process():
            return err

        # Convert pattern to bytes
        if isinstance(pattern, str):
            try:
                pattern_bytes = bytes.fromhex(pattern.replace(" ", ""))
            except ValueError:
                # Try as ASCII
                pattern_bytes = pattern.encode()
        else:
            pattern_bytes = pattern

        # Determine search range
        if start:
            try:
                start_addr = int(start, 16) if start.startswith("0x") else int(start)
            except ValueError:
                return error_response(
                    ErrorType.DEBUGGER_ERROR,
                    f"Invalid start address: {start}",
                    self.state.get_state(),
                )
        else:
            start_addr = 0

        search_size = size or 0x1000000  # Default 16MB

        # Search using LLDB's memory find
        error = self._lldb.SBError()
        matches = []

        # Read in chunks and search
        chunk_size = 0x10000  # 64KB chunks
        current = start_addr

        while current < start_addr + search_size and len(matches) < 100:
            data = self._process.ReadMemory(current, min(chunk_size, start_addr + search_size - current), error)
            if error.Fail() or not data:
                current += chunk_size
                continue

            # Search for pattern in chunk
            offset = 0
            while True:
                idx = data.find(pattern_bytes, offset)
                if idx == -1:
                    break
                matches.append(f"0x{current + idx:x}")
                offset = idx + 1
                if len(matches) >= 100:
                    break

            current += chunk_size

        return ok_response(
            self.state.get_state(),
            {"matches": matches, "pattern": pattern_bytes.hex(), "count": len(matches)},
        )

    def signal_handler(self, signal: str, action: str = "stop") -> Response:
        """Configure signal handling."""
        valid_actions = {"stop", "pass", "ignore", "info"}
        if action not in valid_actions:
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Invalid action: {action}. Use one of: {valid_actions}",
                self.state.get_state(),
            )

        # Use LLDB command for signal handling
        if action == "info":
            result = self._lldb.SBCommandReturnObject()
            self._debugger.GetCommandInterpreter().HandleCommand(
                f"process handle {signal}",
                result,
            )
            return ok_response(
                self.state.get_state(),
                {"signal": signal, "info": result.GetOutput()},
            )

        # Map action to LLDB flags
        flags = {
            "stop": "--stop true --notify true --pass false",
            "pass": "--stop false --notify false --pass true",
            "ignore": "--stop false --notify false --pass false",
        }

        result = self._lldb.SBCommandReturnObject()
        self._debugger.GetCommandInterpreter().HandleCommand(
            f"process handle {signal} {flags[action]}",
            result,
        )

        if not result.Succeeded():
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                result.GetError() or f"Failed to configure signal {signal}",
                self.state.get_state(),
            )

        return ok_response(
            self.state.get_state(),
            {"signal": signal, "action": action},
        )

    def environment(self, action: str = "list", name: str | None = None, value: str | None = None) -> Response:
        """Manage process environment variables."""
        if action == "list":
            if not self._target:
                return ok_response(self.state.get_state(), {"environment": {}})

            env = {}
            launch_info = self._target.GetLaunchInfo()
            env_entries = launch_info.GetEnvironmentEntries()
            for i in range(env_entries.GetSize()):
                entry = env_entries.GetStringAtIndex(i)
                if "=" in entry:
                    k, v = entry.split("=", 1)
                    env[k] = v

            return ok_response(self.state.get_state(), {"environment": env})

        if not name:
            return error_response(
                ErrorType.DEBUGGER_ERROR,
                f"Variable name required for action: {action}",
                self.state.get_state(),
            )

        if action == "get":
            if not self._target:
                return ok_response(self.state.get_state(), {"name": name, "value": None})

            launch_info = self._target.GetLaunchInfo()
            env_entries = launch_info.GetEnvironmentEntries()
            for i in range(env_entries.GetSize()):
                entry = env_entries.GetStringAtIndex(i)
                if entry.startswith(f"{name}="):
                    return ok_response(
                        self.state.get_state(),
                        {"name": name, "value": entry.split("=", 1)[1]},
                    )
            return ok_response(self.state.get_state(), {"name": name, "value": None})

        if action == "set":
            if value is None:
                return error_response(
                    ErrorType.DEBUGGER_ERROR,
                    "Value required for set action",
                    self.state.get_state(),
                )

            result = self._lldb.SBCommandReturnObject()
            self._debugger.GetCommandInterpreter().HandleCommand(
                f'settings set target.env-vars {name}="{value}"',
                result,
            )
            return ok_response(
                self.state.get_state(),
                {"action": "set", "name": name, "value": value},
            )

        if action == "unset":
            result = self._lldb.SBCommandReturnObject()
            self._debugger.GetCommandInterpreter().HandleCommand(
                f"settings remove target.env-vars {name}",
                result,
            )
            return ok_response(
                self.state.get_state(),
                {"action": "unset", "name": name},
            )

        return error_response(
            ErrorType.DEBUGGER_ERROR,
            f"Unknown action: {action}. Use list, get, set, or unset",
            self.state.get_state(),
        )
