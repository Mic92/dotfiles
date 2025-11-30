"""RR-specific utilities for trace management.

This module provides functions to record, list, and manage RR traces
outside of a debugging session.
"""

from __future__ import annotations

import json
import os
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Any


@dataclass
class TraceInfo:
    """Information about an RR trace."""

    name: str
    path: str
    executable: str
    recorded_at: str | None
    size_bytes: int
    event_count: int | None
    cmdline: list[str]

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "path": self.path,
            "executable": self.executable,
            "recorded_at": self.recorded_at,
            "size_bytes": self.size_bytes,
            "event_count": self.event_count,
            "cmdline": self.cmdline,
        }


def get_rr_trace_dir() -> Path:
    """Get the default RR trace directory."""
    return Path(os.environ.get("RR_TRACE_DIR", Path.home() / ".local/share/rr"))


def list_traces() -> list[TraceInfo]:
    """List all available RR traces."""
    trace_dir = get_rr_trace_dir()
    traces = []

    if not trace_dir.exists():
        return traces

    for entry in sorted(trace_dir.iterdir(), key=lambda p: p.stat().st_mtime, reverse=True):
        if entry.is_dir():
            info = get_trace_info(str(entry))
            if info:
                traces.append(info)

    return traces


def get_trace_info(trace_path: str) -> TraceInfo | None:
    """Get information about a specific trace."""
    path = Path(trace_path)

    # Handle trace name vs full path
    if not path.exists():
        path = get_rr_trace_dir() / trace_path

    if not path.exists():
        return None

    # Check for version file (indicates valid RR trace)
    version_file = path / "version"
    if not version_file.exists():
        return None

    # Get executable from symlink or mmaps
    exe = "<unknown>"
    exe_link = path / "exe"
    if exe_link.exists():
        try:
            exe = str(exe_link.resolve())
        except OSError:
            pass

    # Get cmdline if available
    cmdline: list[str] = []
    cmdline_file = path / "cmdline"
    if cmdline_file.exists():
        try:
            cmdline = cmdline_file.read_text().strip().split("\x00")
            cmdline = [c for c in cmdline if c]  # Remove empty strings
        except OSError:
            pass

    # Get recorded timestamp from version file mtime
    recorded_at = None
    try:
        from datetime import datetime

        mtime = version_file.stat().st_mtime
        recorded_at = datetime.fromtimestamp(mtime).isoformat()
    except OSError:
        pass

    # Calculate total size
    size_bytes = 0
    try:
        for f in path.rglob("*"):
            if f.is_file():
                size_bytes += f.stat().st_size
    except OSError:
        pass

    # Try to get event count from rr
    event_count = None
    try:
        result = subprocess.run(
            ["rr", "ps", str(path)],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if result.returncode == 0:
            # Parse output to count events (rough estimate)
            lines = result.stdout.strip().split("\n")
            if len(lines) > 1:  # Skip header
                event_count = len(lines) - 1
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass

    return TraceInfo(
        name=path.name,
        path=str(path),
        executable=exe,
        recorded_at=recorded_at,
        size_bytes=size_bytes,
        event_count=event_count,
        cmdline=cmdline,
    )


def record_trace(
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
    rr_path = shutil.which("rr")
    if not rr_path:
        return {"status": "error", "error": "RR not found in PATH"}

    binary_path = Path(binary)
    if not binary_path.exists():
        # Try to find in PATH
        found = shutil.which(binary)
        if found:
            binary_path = Path(found)
        else:
            return {"status": "error", "error": f"Binary not found: {binary}"}

    cmd = [rr_path, "record"]

    if output_name:
        cmd.extend(["-o", output_name])

    cmd.append(str(binary_path))
    if args:
        cmd.extend(args)

    # Prepare environment
    proc_env = os.environ.copy()
    if env:
        proc_env.update(env)

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            env=proc_env,
            timeout=3600,  # 1 hour timeout
        )

        if result.returncode != 0:
            return {
                "status": "error",
                "error": f"Recording failed: {result.stderr}",
                "stdout": result.stdout,
            }

        # Find the trace that was created
        # RR prints "rr: Saving execution to trace directory `...`"
        trace_path = None
        for line in result.stderr.split("\n"):
            if "Saving execution to trace directory" in line:
                # Extract path from backticks
                start = line.find("`")
                end = line.rfind("`")
                if start != -1 and end != -1 and end > start:
                    trace_path = line[start + 1 : end]
                    break

        if trace_path:
            info = get_trace_info(trace_path)
            if info:
                return {
                    "status": "ok",
                    "trace": info.to_dict(),
                    "output": result.stderr,
                }

        return {
            "status": "ok",
            "output": result.stderr,
            "stdout": result.stdout,
        }

    except subprocess.TimeoutExpired:
        return {"status": "error", "error": "Recording timed out"}
    except Exception as e:
        return {"status": "error", "error": str(e)}


def delete_trace(trace_path: str) -> dict[str, Any]:
    """Delete an RR trace.

    Args:
        trace_path: Path or name of trace to delete

    Returns:
        Dict with status
    """
    path = Path(trace_path)

    if not path.exists():
        path = get_rr_trace_dir() / trace_path

    if not path.exists():
        return {"status": "error", "error": f"Trace not found: {trace_path}"}

    # Verify it's a valid trace (has version file)
    if not (path / "version").exists():
        return {"status": "error", "error": f"Not a valid RR trace: {trace_path}"}

    try:
        shutil.rmtree(path)
        return {"status": "ok", "deleted": str(path)}
    except Exception as e:
        return {"status": "error", "error": str(e)}


def get_trace_events(trace_path: str) -> dict[str, Any]:
    """Get list of events/processes in a trace.

    Args:
        trace_path: Path or name of trace

    Returns:
        Dict with process/event information
    """
    path = Path(trace_path)

    if not path.exists():
        path = get_rr_trace_dir() / trace_path

    if not path.exists():
        return {"status": "error", "error": f"Trace not found: {trace_path}"}

    try:
        result = subprocess.run(
            ["rr", "ps", str(path)],
            capture_output=True,
            text=True,
            timeout=10,
        )

        if result.returncode != 0:
            return {"status": "error", "error": result.stderr}

        # Parse ps output
        # Format: PID PPID EXIT COMMAND
        processes = []
        lines = result.stdout.strip().split("\n")
        if len(lines) > 1:
            for line in lines[1:]:  # Skip header
                parts = line.split(None, 3)
                if len(parts) >= 4:
                    processes.append({
                        "pid": int(parts[0]),
                        "ppid": int(parts[1]),
                        "exit_code": int(parts[2]) if parts[2] != "-" else None,
                        "command": parts[3],
                    })

        return {"status": "ok", "processes": processes}

    except subprocess.TimeoutExpired:
        return {"status": "error", "error": "Command timed out"}
    except Exception as e:
        return {"status": "error", "error": str(e)}
