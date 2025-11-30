"""JSON protocol definitions for debugger-cli.

All communication between client and server uses JSON messages.
This module defines the schemas for requests and responses.
"""

from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class StopReason(Enum):
    """Reason why the debugger stopped."""

    BREAKPOINT = "breakpoint"
    WATCHPOINT = "watchpoint"
    STEP = "step"
    SIGNAL = "signal"
    EXCEPTION = "exception"
    EXITED = "exited"
    NONE = "none"


class ErrorType(Enum):
    """Typed error categories for LLM agents."""

    INVALID_COMMAND = "invalid_command"
    PARSE_ERROR = "parse_error"
    DEBUGGER_ERROR = "debugger_error"
    TARGET_ERROR = "target_error"
    SESSION_ERROR = "session_error"
    NOT_RUNNING = "not_running"
    ALREADY_RUNNING = "already_running"
    BACKEND_NOT_AVAILABLE = "backend_not_available"


@dataclass
class SourceLine:
    """A line of source code with metadata."""

    line_number: int
    text: str
    is_current: bool = False

    def to_dict(self) -> dict[str, Any]:
        return {
            "line_number": self.line_number,
            "text": self.text,
            "is_current": self.is_current,
        }


@dataclass
class Frame:
    """A stack frame."""

    index: int
    function: str
    file: str | None
    line: int | None
    address: str
    module: str | None = None
    args: list[dict[str, str]] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        return {
            "index": self.index,
            "function": self.function,
            "file": self.file,
            "line": self.line,
            "address": self.address,
            "module": self.module,
            "args": self.args,
        }


@dataclass
class Thread:
    """A thread in the target process."""

    id: int
    name: str | None
    is_current: bool
    stop_reason: StopReason
    frame: Frame | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "name": self.name,
            "is_current": self.is_current,
            "stop_reason": self.stop_reason.value,
            "frame": self.frame.to_dict() if self.frame else None,
        }


@dataclass
class Breakpoint:
    """A breakpoint."""

    id: int
    location: str
    file: str | None
    line: int | None
    address: str | None
    enabled: bool
    hit_count: int
    condition: str | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "location": self.location,
            "file": self.file,
            "line": self.line,
            "address": self.address,
            "enabled": self.enabled,
            "hit_count": self.hit_count,
            "condition": self.condition,
        }


@dataclass
class Variable:
    """A variable (local, argument, or expression result)."""

    name: str
    value: str
    type: str
    children: list["Variable"] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "value": self.value,
            "type": self.type,
            "children": [c.to_dict() for c in self.children],
        }


@dataclass
class Register:
    """A CPU register."""

    name: str
    value: str

    def to_dict(self) -> dict[str, Any]:
        return {"name": self.name, "value": self.value}


@dataclass
class MemoryRegion:
    """A region of memory."""

    address: str
    size: int
    data: str  # Hex encoded
    ascii: str | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "address": self.address,
            "size": self.size,
            "data": self.data,
            "ascii": self.ascii,
        }


@dataclass
class DisassemblyLine:
    """A line of disassembly."""

    address: str
    opcode: str
    operands: str
    is_current: bool = False
    symbol: str | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "address": self.address,
            "opcode": self.opcode,
            "operands": self.operands,
            "is_current": self.is_current,
            "symbol": self.symbol,
        }


@dataclass
class DebuggerState:
    """Complete debugger state returned with every response."""

    running: bool
    pid: int | None
    executable: str | None
    threads: list[Thread]
    current_thread_id: int | None
    current_frame: Frame | None
    source_context: list[SourceLine]
    breakpoints: list[Breakpoint]
    backend: str  # "lldb" or "rr"

    def to_dict(self) -> dict[str, Any]:
        return {
            "running": self.running,
            "pid": self.pid,
            "executable": self.executable,
            "threads": [t.to_dict() for t in self.threads],
            "current_thread_id": self.current_thread_id,
            "current_frame": self.current_frame.to_dict() if self.current_frame else None,
            "source_context": [s.to_dict() for s in self.source_context],
            "breakpoints": [b.to_dict() for b in self.breakpoints],
            "backend": self.backend,
        }


@dataclass
class ErrorInfo:
    """Structured error information."""

    type: ErrorType
    message: str
    details: dict[str, Any] | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "type": self.type.value,
            "message": self.message,
            "details": self.details,
        }


@dataclass
class Response:
    """Response from debugger server.

    Every response includes:
    - status: "ok", "stopped", or "error"
    - state: Full debugger state (always present)
    - stop_reason: Why we stopped (if status is "stopped")
    - result: Command-specific result data
    - error: Structured error info (if status is "error")
    """

    status: str  # "ok", "stopped", "error"
    state: DebuggerState
    stop_reason: StopReason = StopReason.NONE
    result: dict[str, Any] | None = None
    error: ErrorInfo | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "status": self.status,
            "state": self.state.to_dict(),
            "stop_reason": self.stop_reason.value,
            "result": self.result,
            "error": self.error.to_dict() if self.error else None,
        }


@dataclass
class Request:
    """Request to debugger server."""

    command: str
    args: list[str] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        return {
            "command": self.command,
            "args": self.args,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "Request":
        return cls(
            command=data.get("command", ""),
            args=data.get("args", []),
        )


def empty_state(backend: str = "lldb") -> DebuggerState:
    """Create an empty debugger state."""
    return DebuggerState(
        running=False,
        pid=None,
        executable=None,
        threads=[],
        current_thread_id=None,
        current_frame=None,
        source_context=[],
        breakpoints=[],
        backend=backend,
    )


def error_response(
    error_type: ErrorType,
    message: str,
    state: DebuggerState | None = None,
    details: dict[str, Any] | None = None,
) -> Response:
    """Create an error response."""
    return Response(
        status="error",
        state=state or empty_state(),
        error=ErrorInfo(type=error_type, message=message, details=details),
    )


def ok_response(
    state: DebuggerState,
    result: dict[str, Any] | None = None,
) -> Response:
    """Create a success response."""
    return Response(
        status="ok",
        state=state,
        result=result,
    )


def stopped_response(
    state: DebuggerState,
    stop_reason: StopReason,
    result: dict[str, Any] | None = None,
) -> Response:
    """Create a stopped response."""
    return Response(
        status="stopped",
        state=state,
        stop_reason=stop_reason,
        result=result,
    )
