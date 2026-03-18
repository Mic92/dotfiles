"""Main entry point for live-text.

Modes:
  live-text                  # Screenshot → OCR + annotation overlay
  live-text --region         # Select region → OCR + annotation overlay
  live-text --window         # Pick a window → OCR + annotation overlay
  live-text image.png        # OCR an existing image
  grim - | live-text -       # Read PNG from stdin
"""

from __future__ import annotations

import argparse
import contextlib
import subprocess
import sys
import tempfile
import threading
from pathlib import Path

from .overlay import LiveTextOverlay
from .screenshot import capture_full_screen, get_focused_output, get_window_geometries


def _tmp_png(stack: contextlib.ExitStack) -> Path:
    """Create a temporary PNG file that is deleted when the stack exits."""
    tmp = tempfile.NamedTemporaryFile(suffix=".png", prefix="live-text-", delete=False)
    tmp.close()
    path = Path(tmp.name)
    stack.callback(path.unlink, missing_ok=True)
    return path


def _read_stdin(stack: contextlib.ExitStack) -> Path:
    """Read binary data from stdin into a temporary PNG file."""
    data = sys.stdin.buffer.read()
    if not data:
        print("Error: no data received on stdin", file=sys.stderr)
        sys.exit(1)
    path = _tmp_png(stack)
    path.write_bytes(data)
    return path


def _capture_geometry(
    stack: contextlib.ExitStack, grim_cmd: str, geometry: str
) -> Path:
    """Capture a screen region with grim into a temporary file."""
    path = _tmp_png(stack)
    try:
        subprocess.run(
            [grim_cmd, "-g", geometry, str(path)],
            check=True,
            capture_output=True,
        )
    except FileNotFoundError as e:
        print(f"Error: required tool not found: {e}", file=sys.stderr)
        sys.exit(1)
    except subprocess.CalledProcessError as e:
        print(f"Error capturing screenshot: {e}", file=sys.stderr)
        sys.exit(1)
    return path


def _slurp_geometry(slurp_cmd: str, stdin: str | None = None) -> str:
    """Run slurp and return the selected geometry string.

    Args:
        slurp_cmd: Path to the slurp binary.
        stdin: Optional predefined rectangles to feed to slurp.
    """
    try:
        result = subprocess.run(
            [slurp_cmd],
            capture_output=True,
            text=True,
            input=stdin,
            check=True,
        )
        return result.stdout.strip()
    except FileNotFoundError:
        print("Error: slurp not found", file=sys.stderr)
        sys.exit(1)
    except subprocess.CalledProcessError:
        sys.exit(0)  # user cancelled


def _capture_screenshot(stack: contextlib.ExitStack, args: argparse.Namespace) -> Path:
    """Resolve screenshot from image arg, stdin, or grim."""
    if args.image == "-":
        return _read_stdin(stack)

    if args.image is not None:
        path = Path(args.image)
        if not path.exists():
            print(f"Error: file not found: {path}", file=sys.stderr)
            sys.exit(1)
        return path

    try:
        output = args.output or get_focused_output()
        path = capture_full_screen(grim_cmd=args.grim, output=output)
    except FileNotFoundError as e:
        print(f"Error: required tool not found: {e}", file=sys.stderr)
        sys.exit(1)
    except subprocess.CalledProcessError as e:
        print(f"Error capturing screenshot: {e}", file=sys.stderr)
        sys.exit(1)
    stack.callback(path.unlink, missing_ok=True)
    return path


def _capture_region(stack: contextlib.ExitStack, args: argparse.Namespace) -> Path:
    """Select a region with slurp and capture it."""
    geometry = _slurp_geometry(args.slurp)
    return _capture_geometry(stack, args.grim, geometry)


def _capture_window(stack: contextlib.ExitStack, args: argparse.Namespace) -> Path:
    """Pick a window via slurp and capture it."""
    windows = get_window_geometries()
    if not windows:
        print("Error: could not get window list from compositor", file=sys.stderr)
        sys.exit(1)

    slurp_input = "\n".join(windows) + "\n"
    geometry = _slurp_geometry(args.slurp, stdin=slurp_input)

    # Strip the label from slurp output (format: "x,y wxh label")
    parts = geometry.split(" ", 2)
    geometry = f"{parts[0]} {parts[1]}" if len(parts) >= 2 else geometry

    return _capture_geometry(stack, args.grim, geometry)


def _run_overlay(screenshot_path: Path, wl_copy_cmd: str) -> None:
    """Open the overlay UI with background OCR and barcode scanning."""
    overlay = LiveTextOverlay(
        screenshot_path=screenshot_path,
        lines=[],
        wl_copy_cmd=wl_copy_cmd,
    )

    def _ocr_worker() -> None:
        from .ocr import run_ocr

        try:
            lines = run_ocr(screenshot_path)
        except Exception as e:
            print(f"OCR error: {e}", file=sys.stderr)
            lines = []
        overlay.set_lines(lines)

    def _barcode_worker() -> None:
        from .barcode import scan_codes

        try:
            codes = scan_codes(screenshot_path)
        except Exception as e:
            print(f"Barcode scan error: {e}", file=sys.stderr)
            codes = []
        overlay.set_codes(codes)

    ocr_thread = threading.Thread(target=_ocr_worker, daemon=True)
    barcode_thread = threading.Thread(target=_barcode_worker, daemon=True)
    ocr_thread.start()
    barcode_thread.start()
    overlay.run()


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Screenshot toolkit: OCR text selection + annotation + region capture",
    )
    parser.add_argument(
        "image",
        nargs="?",
        default=None,
        help="Image file, or '-' for stdin (default: capture full screen)",
    )
    parser.add_argument(
        "--region",
        action="store_true",
        help="Select region with slurp, then open overlay UI",
    )
    parser.add_argument(
        "--window",
        action="store_true",
        help="Pick a window with slurp, then open overlay UI",
    )
    parser.add_argument("--grim", default="grim")
    parser.add_argument("--slurp", default="slurp")
    parser.add_argument("--wl-copy", default="wl-copy")
    parser.add_argument("--notify-send", default="notify-send")
    parser.add_argument("--output", default=None)

    args = parser.parse_args()

    with contextlib.ExitStack() as stack:
        if args.region:
            screenshot_path = _capture_region(stack, args)
        elif args.window:
            screenshot_path = _capture_window(stack, args)
        else:
            screenshot_path = _capture_screenshot(stack, args)

        _run_overlay(screenshot_path, args.wl_copy)


if __name__ == "__main__":
    main()
