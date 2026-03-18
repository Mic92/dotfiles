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


def get_window_geometries() -> list[str]:
    """Get window rectangles in slurp-compatible format.

    Returns a list of strings like "x,y widthxheight title".
    Tries niri first, then swaymsg.  Returns an empty list if
    no compositor is detected.
    """
    # Try niri — if the command succeeds, niri is the compositor, so don't
    # fall through to sway even when no windows are open.
    try:
        result = subprocess.run(
            ["niri", "msg", "--json", "windows"],
            capture_output=True,
            text=True,
            check=True,
        )
        windows = json.loads(result.stdout)
        rects: list[str] = []
        for win in windows:
            x = win.get("x")
            y = win.get("y")
            w = win.get("width")
            h = win.get("height")
            title = win.get("title", "")
            if x is not None and y is not None and w is not None and h is not None:
                rects.append(f"{x},{y} {w}x{h} {title}")
        return rects
    except (subprocess.CalledProcessError, FileNotFoundError, json.JSONDecodeError):
        pass

    # Try sway / sway-compatible compositors
    try:
        result = subprocess.run(
            ["swaymsg", "-t", "get_tree"],
            capture_output=True,
            text=True,
            check=True,
        )
        tree = json.loads(result.stdout)
        rects = []
        _collect_sway_windows(tree, rects)
        if rects:
            return rects
    except (subprocess.CalledProcessError, FileNotFoundError, json.JSONDecodeError):
        pass

    return []


def _collect_sway_windows(node: dict[str, object], out: list[str]) -> None:
    """Recursively collect window rectangles from a sway tree."""
    # A "con" with a name and non-zero rect is a window
    node_type = node.get("type")
    name = node.get("name")
    rect = node.get("rect")
    if (
        node_type in ("con", "floating_con")
        and isinstance(name, str)
        and name
        and isinstance(rect, dict)
    ):
        x = rect.get("x", 0)
        y = rect.get("y", 0)
        w = rect.get("width", 0)
        h = rect.get("height", 0)
        if isinstance(w, int) and isinstance(h, int) and w > 0 and h > 0:
            out.append(f"{x},{y} {w}x{h} {name}")

    nodes = node.get("nodes")
    if isinstance(nodes, list):
        for child in nodes:
            if isinstance(child, dict):
                _collect_sway_windows(child, out)
    floating = node.get("floating_nodes")
    if isinstance(floating, list):
        for child in floating:
            if isinstance(child, dict):
                _collect_sway_windows(child, out)


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
