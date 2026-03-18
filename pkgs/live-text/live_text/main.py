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
import subprocess
import sys
import tempfile
from pathlib import Path

from .screenshot import capture_full_screen, get_focused_output, get_window_geometries


def _notify(notify_send_cmd: str, message: str) -> None:
    """Send a desktop notification, ignoring errors."""
    try:
        subprocess.run(
            [notify_send_cmd, "-t", "3000", "Live Text", message],
            check=False,
        )
    except FileNotFoundError:
        pass


def _read_stdin_to_tempfile() -> Path:
    """Read binary data from stdin and write to a temp PNG file."""
    data = sys.stdin.buffer.read()
    if not data:
        print("Error: no data received on stdin", file=sys.stderr)
        sys.exit(1)
    tmp = tempfile.NamedTemporaryFile(suffix=".png", prefix="live-text-", delete=False)
    tmp.write(data)
    tmp.close()
    return Path(tmp.name)


def _capture_screenshot(args: argparse.Namespace) -> tuple[Path, bool]:
    """Resolve screenshot from image arg, stdin, or grim.

    Returns (path, should_cleanup).
    """
    if args.image == "-":
        return _read_stdin_to_tempfile(), True

    if args.image is not None:
        path = Path(args.image)
        if not path.exists():
            print(f"Error: file not found: {path}", file=sys.stderr)
            sys.exit(1)
        return path, False

    try:
        output = args.output or get_focused_output()
        return capture_full_screen(grim_cmd=args.grim, output=output), True
    except FileNotFoundError as e:
        print(f"Error: required tool not found: {e}", file=sys.stderr)
        sys.exit(1)
    except subprocess.CalledProcessError as e:
        print(f"Error capturing screenshot: {e}", file=sys.stderr)
        sys.exit(1)


def _mode_default(args: argparse.Namespace) -> None:
    """Default mode: OCR + annotation overlay.

    Shows the overlay immediately so the user can start annotating,
    then runs OCR in a background thread and adds text selection
    once detection completes.
    """
    import threading

    from .overlay import LiveTextOverlay

    screenshot_path, cleanup = _capture_screenshot(args)
    try:
        overlay = LiveTextOverlay(
            screenshot_path=screenshot_path,
            lines=[],
            wl_copy_cmd=args.wl_copy,
        )

        def _ocr_worker() -> None:
            from .ocr import run_ocr

            try:
                lines = run_ocr(screenshot_path)
            except Exception as e:
                print(f"OCR error: {e}", file=sys.stderr)
                return
            if lines:
                overlay.set_lines(lines)

        thread = threading.Thread(target=_ocr_worker, daemon=True)
        thread.start()
        overlay.run()
    finally:
        if cleanup and screenshot_path.exists():
            screenshot_path.unlink()


def _capture_region(grim_cmd: str, slurp_cmd: str) -> Path:
    """Select a region with slurp and capture it with grim.

    Returns the path to a temporary PNG file. The caller must clean it up.
    """
    try:
        geometry = subprocess.run(
            [slurp_cmd],
            capture_output=True,
            text=True,
            check=True,
        ).stdout.strip()
    except FileNotFoundError:
        print("Error: slurp not found", file=sys.stderr)
        sys.exit(1)
    except subprocess.CalledProcessError:
        sys.exit(0)  # user cancelled

    tmp = tempfile.NamedTemporaryFile(suffix=".png", prefix="live-text-", delete=False)
    tmp.close()
    path = Path(tmp.name)
    try:
        subprocess.run(
            [grim_cmd, "-g", geometry, str(path)],
            check=True,
            capture_output=True,
        )
    except FileNotFoundError as e:
        path.unlink(missing_ok=True)
        print(f"Error: required tool not found: {e}", file=sys.stderr)
        sys.exit(1)
    except subprocess.CalledProcessError as e:
        path.unlink(missing_ok=True)
        print(f"Error capturing region: {e}", file=sys.stderr)
        sys.exit(1)
    return path


def _capture_window(grim_cmd: str, slurp_cmd: str) -> Path:
    """Let the user pick a window via slurp and capture it with grim.

    Window geometries are fetched from the compositor (niri or sway) and
    fed into slurp as predefined rectangles so the user can click to select.

    Returns the path to a temporary PNG file. The caller must clean it up.
    """
    windows = get_window_geometries()
    if not windows:
        print("Error: could not get window list from compositor", file=sys.stderr)
        sys.exit(1)

    slurp_input = "\n".join(windows) + "\n"
    try:
        geometry = subprocess.run(
            [slurp_cmd],
            capture_output=True,
            text=True,
            input=slurp_input,
            check=True,
        ).stdout.strip()
    except FileNotFoundError:
        print("Error: slurp not found", file=sys.stderr)
        sys.exit(1)
    except subprocess.CalledProcessError:
        sys.exit(0)  # user cancelled

    # Strip the label from slurp output (format: "x,y wxh label")
    # We only need the geometry part for grim.
    parts = geometry.split(" ", 2)
    geometry = f"{parts[0]} {parts[1]}" if len(parts) >= 2 else geometry

    tmp = tempfile.NamedTemporaryFile(suffix=".png", prefix="live-text-", delete=False)
    tmp.close()
    path = Path(tmp.name)
    try:
        subprocess.run(
            [grim_cmd, "-g", geometry, str(path)],
            check=True,
            capture_output=True,
        )
    except FileNotFoundError as e:
        path.unlink(missing_ok=True)
        print(f"Error: required tool not found: {e}", file=sys.stderr)
        sys.exit(1)
    except subprocess.CalledProcessError as e:
        path.unlink(missing_ok=True)
        print(f"Error capturing window: {e}", file=sys.stderr)
        sys.exit(1)
    return path


def _mode_window(args: argparse.Namespace) -> None:
    """Window mode: pick a window, capture it, then open the overlay UI."""
    import threading

    from .overlay import LiveTextOverlay

    screenshot_path = _capture_window(grim_cmd=args.grim, slurp_cmd=args.slurp)
    try:
        overlay = LiveTextOverlay(
            screenshot_path=screenshot_path,
            lines=[],
            wl_copy_cmd=args.wl_copy,
        )

        def _ocr_worker() -> None:
            from .ocr import run_ocr

            try:
                lines = run_ocr(screenshot_path)
            except Exception as e:
                print(f"OCR error: {e}", file=sys.stderr)
                return
            if lines:
                overlay.set_lines(lines)

        thread = threading.Thread(target=_ocr_worker, daemon=True)
        thread.start()
        overlay.run()
    finally:
        if screenshot_path.exists():
            screenshot_path.unlink()


def _mode_region(args: argparse.Namespace) -> None:
    """Region mode: select region, capture it, then open the overlay UI."""
    import threading

    from .overlay import LiveTextOverlay

    screenshot_path = _capture_region(grim_cmd=args.grim, slurp_cmd=args.slurp)
    try:
        overlay = LiveTextOverlay(
            screenshot_path=screenshot_path,
            lines=[],
            wl_copy_cmd=args.wl_copy,
        )

        def _ocr_worker() -> None:
            from .ocr import run_ocr

            try:
                lines = run_ocr(screenshot_path)
            except Exception as e:
                print(f"OCR error: {e}", file=sys.stderr)
                return
            if lines:
                overlay.set_lines(lines)

        thread = threading.Thread(target=_ocr_worker, daemon=True)
        thread.start()
        overlay.run()
    finally:
        if screenshot_path.exists():
            screenshot_path.unlink()


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

    if args.region:
        _mode_region(args)
    elif args.window:
        _mode_window(args)
    else:
        _mode_default(args)


if __name__ == "__main__":
    main()
