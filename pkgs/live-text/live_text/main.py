"""Main entry point for live-text: interactive OCR overlay for Wayland."""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

from .ocr import run_ocr
from .overlay import LiveTextOverlay
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


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Interactive OCR overlay - capture screen, detect text, click to copy",
    )
    parser.add_argument(
        "image",
        nargs="?",
        type=Path,
        help="Image file to OCR (default: capture full screen)",
    )
    parser.add_argument(
        "--grim", default="grim", help="Path to grim binary (default: grim)"
    )
    parser.add_argument(
        "--wl-copy",
        default="wl-copy",
        help="Path to wl-copy binary (default: wl-copy)",
    )
    parser.add_argument(
        "--notify-send",
        default="notify-send",
        help="Path to notify-send binary (default: notify-send)",
    )
    parser.add_argument(
        "--output",
        default=None,
        help="Wayland output name to capture (default: auto-detect focused)",
    )

    args = parser.parse_args()

    # Capture or use provided image
    cleanup_screenshot = False
    if args.image is not None:
        screenshot_path = args.image
        if not screenshot_path.exists():
            print(f"Error: file not found: {screenshot_path}", file=sys.stderr)
            sys.exit(1)
    else:
        try:
            output = args.output or get_focused_output()
            screenshot_path = capture_full_screen(
                grim_cmd=args.grim,
                output=output,
            )
            cleanup_screenshot = True
        except FileNotFoundError as e:
            print(f"Error: required tool not found: {e}", file=sys.stderr)
            sys.exit(1)
        except subprocess.CalledProcessError as e:
            print(f"Error capturing screenshot: {e}", file=sys.stderr)
            sys.exit(1)

    try:
        # Run OCR
        try:
            lines = run_ocr(screenshot_path)
        except Exception as e:
            print(f"Error running OCR: {e}", file=sys.stderr)
            sys.exit(1)

        if not lines:
            print("No text detected in screenshot.", file=sys.stderr)
            _notify(args.notify_send, "No text detected in screenshot.")
            sys.exit(0)

        # Show overlay
        overlay = LiveTextOverlay(
            screenshot_path=screenshot_path,
            lines=lines,
            wl_copy_cmd=args.wl_copy,
        )
        overlay.run()

    finally:
        if cleanup_screenshot and screenshot_path.exists():
            screenshot_path.unlink()


if __name__ == "__main__":
    main()
