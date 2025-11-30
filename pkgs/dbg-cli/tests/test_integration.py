"""Integration tests for debugger-cli."""

import json
import os
import shutil
import subprocess
import tempfile
import time
from pathlib import Path

import pytest

from dbg_cli.commands import CommandParseError, CommandType, parse_command
from dbg_cli.protocol import (
    DebuggerState,
    ErrorType,
    Frame,
    Response,
    StopReason,
    empty_state,
    error_response,
    ok_response,
)
from dbg_cli.state import StateManager


class TestCommandParser:
    """Test command parsing."""

    def test_parse_launch(self) -> None:
        cmd = parse_command("launch /bin/ls -la")
        assert cmd.type == CommandType.LAUNCH
        assert cmd.args["binary"] == "/bin/ls"
        assert cmd.args["args"] == ["-la"]

    def test_parse_attach(self) -> None:
        cmd = parse_command("attach 1234")
        assert cmd.type == CommandType.ATTACH
        assert cmd.args["pid"] == 1234

    def test_parse_continue(self) -> None:
        cmd = parse_command("continue")
        assert cmd.type == CommandType.CONTINUE

    def test_parse_continue_alias(self) -> None:
        cmd = parse_command("c")
        assert cmd.type == CommandType.CONTINUE

    def test_parse_step_with_count(self) -> None:
        cmd = parse_command("step 5")
        assert cmd.type == CommandType.STEP
        assert cmd.args["count"] == 5

    def test_parse_step_default_count(self) -> None:
        cmd = parse_command("step")
        assert cmd.type == CommandType.STEP
        assert cmd.args["count"] == 1

    def test_parse_breakpoint_set(self) -> None:
        cmd = parse_command("break main")
        assert cmd.type == CommandType.BREAKPOINT_SET
        assert cmd.args["location"] == "main"

    def test_parse_breakpoint_file_line(self) -> None:
        cmd = parse_command("b test.c:42")
        assert cmd.type == CommandType.BREAKPOINT_SET
        assert cmd.args["location"] == "test.c:42"

    def test_parse_print(self) -> None:
        cmd = parse_command("print x + y")
        assert cmd.type == CommandType.PRINT
        assert cmd.args["expression"] == "x + y"

    def test_parse_memory_read(self) -> None:
        cmd = parse_command("memory 0x1000 64")
        assert cmd.type == CommandType.MEMORY_READ
        assert cmd.args["address"] == "0x1000"
        assert cmd.args["size"] == 64

    def test_parse_empty_command(self) -> None:
        with pytest.raises(CommandParseError):
            parse_command("")

    def test_parse_unknown_command(self) -> None:
        with pytest.raises(CommandParseError):
            parse_command("unknowncommand")

    def test_parse_missing_required_arg(self) -> None:
        with pytest.raises(CommandParseError):
            parse_command("launch")  # Missing binary

    def test_parse_reverse_commands(self) -> None:
        cmd = parse_command("reverse-continue")
        assert cmd.type == CommandType.REVERSE_CONTINUE

        cmd = parse_command("rc")
        assert cmd.type == CommandType.REVERSE_CONTINUE

        cmd = parse_command("reverse-step 3")
        assert cmd.type == CommandType.REVERSE_STEP
        assert cmd.args["count"] == 3


class TestProtocol:
    """Test protocol structures."""

    def test_empty_state(self) -> None:
        state = empty_state("lldb")
        assert state.running is False
        assert state.pid is None
        assert state.backend == "lldb"
        assert state.threads == []

    def test_state_to_dict(self) -> None:
        state = empty_state("rr")
        d = state.to_dict()
        assert d["running"] is False
        assert d["backend"] == "rr"
        assert "threads" in d
        assert "breakpoints" in d

    def test_error_response(self) -> None:
        resp = error_response(ErrorType.NOT_RUNNING, "No process")
        assert resp.status == "error"
        assert resp.error is not None
        assert resp.error.type == ErrorType.NOT_RUNNING
        assert resp.error.message == "No process"

    def test_ok_response(self) -> None:
        state = empty_state()
        resp = ok_response(state, {"test": "value"})
        assert resp.status == "ok"
        assert resp.result == {"test": "value"}
        assert resp.error is None

    def test_response_to_dict(self) -> None:
        state = empty_state()
        resp = ok_response(state, {"key": "value"})
        d = resp.to_dict()
        assert d["status"] == "ok"
        assert d["result"] == {"key": "value"}
        assert "state" in d


class TestStateManager:
    """Test state management."""

    def test_initial_state(self) -> None:
        mgr = StateManager("lldb")
        state = mgr.get_state()
        assert state.running is False
        assert state.pid is None

    def test_set_process(self) -> None:
        mgr = StateManager()
        mgr.set_process(1234, "/bin/ls")
        state = mgr.get_state()
        assert state.pid == 1234
        assert state.executable == "/bin/ls"

    def test_clear_process(self) -> None:
        mgr = StateManager()
        mgr.set_process(1234, "/bin/ls")
        mgr.clear_process()
        state = mgr.get_state()
        assert state.pid is None
        assert state.running is False

    def test_breakpoint_management(self) -> None:
        from dbg_cli.protocol import Breakpoint

        mgr = StateManager()
        bp = Breakpoint(
            id=1,
            location="main",
            file="test.c",
            line=10,
            address="0x1000",
            enabled=True,
            hit_count=0,
        )
        mgr.add_breakpoint(bp)

        state = mgr.get_state()
        assert len(state.breakpoints) == 1
        assert state.breakpoints[0].id == 1

        # Remove
        assert mgr.remove_breakpoint(1) is True
        assert mgr.remove_breakpoint(999) is False
        assert len(mgr.get_state().breakpoints) == 0

    def test_source_context(self) -> None:
        mgr = StateManager()

        # Create a temp file with source
        with tempfile.NamedTemporaryFile(mode="w", suffix=".c", delete=False) as f:
            for i in range(20):
                f.write(f"line {i + 1}\n")
            temp_path = f.name

        try:
            frame = Frame(
                index=0,
                function="test",
                file=temp_path,
                line=10,
                address="0x1000",
            )
            mgr.set_current_frame(frame)

            state = mgr.get_state()
            assert len(state.source_context) > 0

            # Check current line is marked
            current_lines = [s for s in state.source_context if s.is_current]
            assert len(current_lines) == 1
            assert current_lines[0].line_number == 10

        finally:
            os.unlink(temp_path)


class TestClientServer:
    """Test client-server communication (requires pueue)."""

    @pytest.fixture
    def temp_env(self):
        """Create isolated environment for tests."""
        temp_dir = tempfile.mkdtemp()
        old_env = os.environ.copy()

        # Use temp directory for runtime files
        os.environ["XDG_RUNTIME_DIR"] = temp_dir

        yield temp_dir

        # Cleanup
        os.environ.clear()
        os.environ.update(old_env)
        shutil.rmtree(temp_dir, ignore_errors=True)

    @pytest.mark.skipif(
        shutil.which("pueue") is None,
        reason="pueue not installed",
    )
    def test_session_lifecycle(self, temp_env: str) -> None:
        """Test starting, listing, and stopping sessions."""
        from dbg_cli.client import list_sessions, start_session, stop_session

        # Start a session
        session_id = start_session("lldb", "test-session")

        if session_id is None:
            pytest.skip("Could not start session (pueue not running?)")

        try:
            # List sessions
            sessions = list_sessions()
            session_ids = [s["session_id"] for s in sessions]
            assert session_id in session_ids

        finally:
            # Stop session
            stop_session(session_id)

            # Verify stopped
            time.sleep(0.5)
            sessions = list_sessions()
            session_ids = [s["session_id"] for s in sessions]
            # Session should be gone or marked as stopped
            active = [s for s in sessions if s["session_id"] == session_id and s["status"] == "Running"]
            assert len(active) == 0


class TestJSONOutput:
    """Test that all outputs are valid JSON."""

    def test_response_serialization(self) -> None:
        state = empty_state()
        state.pid = 1234

        resp = ok_response(state, {"key": "value"})
        json_str = json.dumps(resp.to_dict())

        # Should be valid JSON
        parsed = json.loads(json_str)
        assert parsed["status"] == "ok"
        assert parsed["state"]["pid"] == 1234

    def test_error_serialization(self) -> None:
        resp = error_response(
            ErrorType.DEBUGGER_ERROR,
            "Test error",
            details={"code": 42},
        )
        json_str = json.dumps(resp.to_dict())

        parsed = json.loads(json_str)
        assert parsed["status"] == "error"
        assert parsed["error"]["type"] == "debugger_error"
        assert parsed["error"]["details"]["code"] == 42

    def test_complex_state_serialization(self) -> None:
        from dbg_cli.protocol import Breakpoint, SourceLine, Thread

        state = DebuggerState(
            running=False,
            pid=1234,
            executable="/bin/test",
            threads=[
                Thread(
                    id=1,
                    name="main",
                    is_current=True,
                    stop_reason=StopReason.BREAKPOINT,
                    frame=Frame(
                        index=0,
                        function="main",
                        file="/test.c",
                        line=10,
                        address="0x1000",
                    ),
                )
            ],
            current_thread_id=1,
            current_frame=Frame(
                index=0,
                function="main",
                file="/test.c",
                line=10,
                address="0x1000",
            ),
            source_context=[
                SourceLine(line_number=9, text="  int x = 1;", is_current=False),
                SourceLine(line_number=10, text="  return x;", is_current=True),
            ],
            breakpoints=[
                Breakpoint(
                    id=1,
                    location="main",
                    file="/test.c",
                    line=10,
                    address="0x1000",
                    enabled=True,
                    hit_count=1,
                )
            ],
            backend="lldb",
        )

        json_str = json.dumps(state.to_dict())
        parsed = json.loads(json_str)

        assert parsed["pid"] == 1234
        assert len(parsed["threads"]) == 1
        assert parsed["threads"][0]["stop_reason"] == "breakpoint"
        assert len(parsed["source_context"]) == 2
        assert parsed["breakpoints"][0]["hit_count"] == 1
