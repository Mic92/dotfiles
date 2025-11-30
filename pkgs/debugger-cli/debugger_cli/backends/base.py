"""Abstract base class for debugger backends."""

from abc import ABC, abstractmethod

from debugger_cli.protocol import (
    Breakpoint,
    DisassemblyLine,
    Frame,
    MemoryRegion,
    Register,
    Response,
    StopReason,
    Thread,
    Variable,
)
from debugger_cli.state import StateManager


class DebuggerBackend(ABC):
    """Abstract interface for debugger backends (LLDB, RR, etc.)."""

    def __init__(self, state: StateManager) -> None:
        """Initialize backend with state manager."""
        self.state = state

    @property
    @abstractmethod
    def name(self) -> str:
        """Backend name (e.g., 'lldb', 'rr')."""
        ...

    @property
    @abstractmethod
    def supports_reverse(self) -> bool:
        """Whether this backend supports reverse debugging."""
        ...

    # Session management

    @abstractmethod
    def launch(self, binary: str, args: list[str]) -> Response:
        """Launch a new process.

        Args:
            binary: Path to executable
            args: Command line arguments

        Returns:
            Response with updated state
        """
        ...

    @abstractmethod
    def attach(self, pid: int) -> Response:
        """Attach to a running process.

        Args:
            pid: Process ID to attach to

        Returns:
            Response with updated state
        """
        ...

    @abstractmethod
    def detach(self) -> Response:
        """Detach from current process.

        Returns:
            Response with updated state
        """
        ...

    @abstractmethod
    def quit(self) -> Response:
        """Quit the debugger.

        Returns:
            Response with final state
        """
        ...

    # Execution control

    @abstractmethod
    def continue_execution(self) -> Response:
        """Continue execution until next stop.

        Returns:
            Response with stop reason and updated state
        """
        ...

    @abstractmethod
    def step(self, count: int = 1) -> Response:
        """Step into (source level).

        Args:
            count: Number of steps

        Returns:
            Response with updated state
        """
        ...

    @abstractmethod
    def next(self, count: int = 1) -> Response:
        """Step over (source level).

        Args:
            count: Number of steps

        Returns:
            Response with updated state
        """
        ...

    @abstractmethod
    def finish(self) -> Response:
        """Run until current function returns.

        Returns:
            Response with updated state
        """
        ...

    def reverse_continue(self) -> Response:
        """Reverse continue (reverse debugging).

        Default implementation returns error for backends that don't support it.
        """
        from debugger_cli.protocol import ErrorType, error_response

        return error_response(
            ErrorType.DEBUGGER_ERROR,
            f"Backend '{self.name}' does not support reverse debugging",
            self.state.get_state(),
        )

    def reverse_step(self, count: int = 1) -> Response:
        """Reverse step (reverse debugging).

        Default implementation returns error for backends that don't support it.
        """
        from debugger_cli.protocol import ErrorType, error_response

        return error_response(
            ErrorType.DEBUGGER_ERROR,
            f"Backend '{self.name}' does not support reverse debugging",
            self.state.get_state(),
        )

    def reverse_next(self, count: int = 1) -> Response:
        """Reverse next (reverse debugging).

        Default implementation returns error for backends that don't support it.
        """
        from debugger_cli.protocol import ErrorType, error_response

        return error_response(
            ErrorType.DEBUGGER_ERROR,
            f"Backend '{self.name}' does not support reverse debugging",
            self.state.get_state(),
        )

    # Breakpoints

    @abstractmethod
    def breakpoint_set(self, location: str) -> Response:
        """Set a breakpoint.

        Args:
            location: Breakpoint location (file:line, function, address)

        Returns:
            Response with breakpoint info
        """
        ...

    @abstractmethod
    def breakpoint_delete(self, bp_id: int) -> Response:
        """Delete a breakpoint.

        Args:
            bp_id: Breakpoint ID

        Returns:
            Response with updated state
        """
        ...

    @abstractmethod
    def breakpoint_list(self) -> Response:
        """List all breakpoints.

        Returns:
            Response with breakpoint list
        """
        ...

    @abstractmethod
    def breakpoint_enable(self, bp_id: int) -> Response:
        """Enable a breakpoint.

        Args:
            bp_id: Breakpoint ID

        Returns:
            Response with updated state
        """
        ...

    @abstractmethod
    def breakpoint_disable(self, bp_id: int) -> Response:
        """Disable a breakpoint.

        Args:
            bp_id: Breakpoint ID

        Returns:
            Response with updated state
        """
        ...

    @abstractmethod
    def watchpoint_set(self, expression: str) -> Response:
        """Set a watchpoint on an expression.

        Args:
            expression: Expression to watch

        Returns:
            Response with watchpoint info
        """
        ...

    # Inspection

    @abstractmethod
    def backtrace(self, count: int | None = None) -> Response:
        """Get stack backtrace.

        Args:
            count: Maximum frames to return (None for all)

        Returns:
            Response with frame list
        """
        ...

    @abstractmethod
    def frame_select(self, index: int) -> Response:
        """Select a stack frame.

        Args:
            index: Frame index

        Returns:
            Response with frame info
        """
        ...

    @abstractmethod
    def thread_select(self, thread_id: int) -> Response:
        """Select a thread.

        Args:
            thread_id: Thread ID

        Returns:
            Response with thread info
        """
        ...

    @abstractmethod
    def thread_list(self) -> Response:
        """List all threads.

        Returns:
            Response with thread list
        """
        ...

    @abstractmethod
    def locals(self) -> Response:
        """Get local variables.

        Returns:
            Response with variable list
        """
        ...

    @abstractmethod
    def args(self) -> Response:
        """Get function arguments.

        Returns:
            Response with argument list
        """
        ...

    @abstractmethod
    def print_expr(self, expression: str) -> Response:
        """Evaluate and print an expression.

        Args:
            expression: Expression to evaluate

        Returns:
            Response with expression result
        """
        ...

    @abstractmethod
    def memory_read(self, address: str, size: int) -> Response:
        """Read memory.

        Args:
            address: Start address (hex or symbol)
            size: Number of bytes to read

        Returns:
            Response with memory contents
        """
        ...

    @abstractmethod
    def registers(self) -> Response:
        """Get register values.

        Returns:
            Response with register list
        """
        ...

    # Source and disassembly

    @abstractmethod
    def source(self, location: str | None = None) -> Response:
        """Get source code around location.

        Args:
            location: Location (default: current)

        Returns:
            Response with source lines
        """
        ...

    @abstractmethod
    def disassemble(self, location: str | None = None) -> Response:
        """Disassemble code at location.

        Args:
            location: Location (default: current)

        Returns:
            Response with disassembly lines
        """
        ...

    # Status

    @abstractmethod
    def status(self) -> Response:
        """Get current debugger status.

        Returns:
            Response with current state
        """
        ...

    # Internal helpers

    def _update_threads(self) -> list[Thread]:
        """Update and return thread list. To be implemented by backends."""
        return []

    def _get_current_frame(self) -> Frame | None:
        """Get current stack frame. To be implemented by backends."""
        return None

    def _refresh_state(self) -> None:
        """Refresh all state from debugger."""
        threads = self._update_threads()
        self.state.set_threads(threads)
        frame = self._get_current_frame()
        self.state.set_current_frame(frame)
