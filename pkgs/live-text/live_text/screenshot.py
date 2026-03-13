"""Screenshot capture using grim (Wayland)."""

from __future__ import annotations

import json
import subprocess
import tempfile
from pathlib import Path


def get_focused_output() -> str | None:
    """Detect the focused Wayland output name.

    Tries niri first, then falls back to swaymsg for sway compatibility.
    Returns None if detection fails (grim will then capture all outputs).
    """
    # Try niri
    try:
        result = subprocess.run(
            ["niri", "msg", "--json", "focused-output"],
            capture_output=True,
            text=True,
            check=True,
        )
        data = json.loads(result.stdout)
        name = data.get("name")
        if isinstance(name, str) and name:
            return name
    except (subprocess.CalledProcessError, FileNotFoundError, json.JSONDecodeError):
        pass

    # Try sway / sway-compatible compositors
    try:
        result = subprocess.run(
            ["swaymsg", "-t", "get_outputs"],
            capture_output=True,
            text=True,
            check=True,
        )
        for output in json.loads(result.stdout):
            if output.get("focused"):
                name = output.get("name")
                if isinstance(name, str) and name:
                    return name
    except (subprocess.CalledProcessError, FileNotFoundError, json.JSONDecodeError):
        pass

    return None


def capture_full_screen(
    grim_cmd: str = "grim",
    output: str | None = None,
) -> Path:
    """Capture a screenshot using grim.

    Args:
        grim_cmd: Path to the grim binary.
        output: Wayland output name (e.g. "eDP-1") to capture a single
            monitor.  When None, grim captures all outputs stitched together.

    Returns the path to the temporary PNG file.
    The caller is responsible for cleaning up the file.
    """
    tmp = tempfile.NamedTemporaryFile(suffix=".png", prefix="live-text-", delete=False)
    tmp.close()
    path = Path(tmp.name)

    cmd = [grim_cmd]
    if output is not None:
        cmd += ["-o", output]
    cmd.append(str(path))

    try:
        subprocess.run(
            cmd,
            check=True,
            capture_output=True,
        )
    except Exception:
        # Don't leak the temp file when grim fails
        path.unlink(missing_ok=True)
        raise

    return path
