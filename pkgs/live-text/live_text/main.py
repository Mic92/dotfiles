"""Main entry point for live-text.

Modes:
  live-text                  # Screenshot → OCR + annotation overlay
  live-text --region         # Select region → copy to clipboard
  live-text image.png        # OCR an existing image
  grim - | live-text -       # Read PNG from stdin
"""

from __future__ import annotations

import argparse
import subprocess
import sys
import tempfile
from pathlib import Path

from .screenshot import capture_full_screen, get_focused_output


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


def _mode_region(args: argparse.Namespace) -> None:
    """Region mode: select region with slurp, copy to clipboard."""
    try:
        geometry = subprocess.run(
            [args.slurp],
            capture_output=True,
            text=True,
            check=True,
        ).stdout.strip()
    except FileNotFoundError:
        print("Error: slurp not found", file=sys.stderr)
        sys.exit(1)
    except subprocess.CalledProcessError:
        sys.exit(0)  # user cancelled

    try:
        grim = subprocess.Popen(
            [args.grim, "-g", geometry, "-"],
            stdout=subprocess.PIPE,
        )
        subprocess.run(
            [args.wl_copy, "-t", "image/png"],
            stdin=grim.stdout,
            check=True,
        )
        grim.wait()
        _notify(args.notify_send, "Region copied to clipboard")
    except FileNotFoundError as e:
        print(f"Error: required tool not found: {e}", file=sys.stderr)
        sys.exit(1)
    except subprocess.CalledProcessError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


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
        help="Select region with slurp, copy to clipboard",
    )
    parser.add_argument("--grim", default="grim")
    parser.add_argument("--slurp", default="slurp")
    parser.add_argument("--wl-copy", default="wl-copy")
    parser.add_argument("--notify-send", default="notify-send")
    parser.add_argument("--output", default=None)

    args = parser.parse_args()

    if args.region:
        _mode_region(args)
    else:
        _mode_default(args)


if __name__ == "__main__":
    main()
