"""Debugger state management.

Maintains and updates the debugger state representation.
"""

from pathlib import Path

from debugger_cli.protocol import (
    Breakpoint,
    DebuggerState,
    Frame,
    SourceLine,
    StopReason,
    Thread,
)


class StateManager:
    """Manages debugger state and provides state snapshots."""

    def __init__(self, backend: str = "lldb") -> None:
        self.backend = backend
        self._running = False
        self._pid: int | None = None
        self._executable: str | None = None
        self._threads: list[Thread] = []
        self._current_thread_id: int | None = None
        self._current_frame: Frame | None = None
        self._source_context: list[SourceLine] = []
        self._breakpoints: list[Breakpoint] = []
        self._source_cache: dict[str, list[str]] = {}

    def get_state(self) -> DebuggerState:
        """Get current debugger state snapshot."""
        return DebuggerState(
            running=self._running,
            pid=self._pid,
            executable=self._executable,
            threads=self._threads.copy(),
            current_thread_id=self._current_thread_id,
            current_frame=self._current_frame,
            source_context=self._source_context.copy(),
            breakpoints=self._breakpoints.copy(),
            backend=self.backend,
        )

    def set_running(self, running: bool) -> None:
        """Set whether the target is running."""
        self._running = running

    def set_process(self, pid: int | None, executable: str | None) -> None:
        """Set process information."""
        self._pid = pid
        self._executable = executable

    def clear_process(self) -> None:
        """Clear process state (after exit/detach)."""
        self._pid = None
        self._running = False
        self._threads = []
        self._current_thread_id = None
        self._current_frame = None
        self._source_context = []

    def set_threads(self, threads: list[Thread]) -> None:
        """Set thread list."""
        self._threads = threads

    def set_current_thread(self, thread_id: int | None) -> None:
        """Set current thread ID."""
        self._current_thread_id = thread_id

    def set_current_frame(self, frame: Frame | None) -> None:
        """Set current frame and update source context."""
        self._current_frame = frame
        if frame and frame.file and frame.line:
            self._update_source_context(frame.file, frame.line)
        else:
            self._source_context = []

    def _update_source_context(
        self, file_path: str, current_line: int, context_lines: int = 5
    ) -> None:
        """Update source context around current line."""
        self._source_context = []

        # Try to read source file
        lines = self._get_source_lines(file_path)
        if not lines:
            return

        # Calculate range
        start = max(0, current_line - context_lines - 1)
        end = min(len(lines), current_line + context_lines)

        for i in range(start, end):
            line_num = i + 1
            self._source_context.append(
                SourceLine(
                    line_number=line_num,
                    text=lines[i].rstrip("\n\r"),
                    is_current=(line_num == current_line),
                )
            )

    def _get_source_lines(self, file_path: str) -> list[str]:
        """Get source lines from cache or file."""
        if file_path in self._source_cache:
            return self._source_cache[file_path]

        try:
            path = Path(file_path)
            if path.exists():
                lines = path.read_text().splitlines(keepends=True)
                self._source_cache[file_path] = lines
                return lines
        except (OSError, UnicodeDecodeError):
            pass

        return []

    def clear_source_cache(self) -> None:
        """Clear the source file cache."""
        self._source_cache.clear()

    def set_breakpoints(self, breakpoints: list[Breakpoint]) -> None:
        """Set breakpoint list."""
        self._breakpoints = breakpoints

    def add_breakpoint(self, breakpoint: Breakpoint) -> None:
        """Add a breakpoint."""
        self._breakpoints.append(breakpoint)

    def remove_breakpoint(self, breakpoint_id: int) -> bool:
        """Remove a breakpoint by ID. Returns True if found."""
        for i, bp in enumerate(self._breakpoints):
            if bp.id == breakpoint_id:
                del self._breakpoints[i]
                return True
        return False

    def get_breakpoint(self, breakpoint_id: int) -> Breakpoint | None:
        """Get a breakpoint by ID."""
        for bp in self._breakpoints:
            if bp.id == breakpoint_id:
                return bp
        return None

    def update_stop_state(
        self,
        thread_id: int,
        stop_reason: StopReason,
        frame: Frame,
    ) -> None:
        """Update state when debugger stops."""
        self._running = False
        self._current_thread_id = thread_id
        self.set_current_frame(frame)

        # Update thread stop reason
        for thread in self._threads:
            if thread.id == thread_id:
                thread.stop_reason = stop_reason
                thread.is_current = True
                thread.frame = frame
            else:
                thread.is_current = False

    def reset(self) -> None:
        """Reset all state."""
        self._running = False
        self._pid = None
        self._executable = None
        self._threads = []
        self._current_thread_id = None
        self._current_frame = None
        self._source_context = []
        self._breakpoints = []
        self._source_cache.clear()
