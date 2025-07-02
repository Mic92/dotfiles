#!/usr/bin/env python3
"""
Claude Code Hook for tmux notifications
Rings bell and displays message when Claude needs user input
"""

import json
import os
import subprocess
import sys
from datetime import UTC, datetime
from pathlib import Path


class ClaudeTmuxNotifier:
    def __init__(self) -> None:
        self.in_tmux = os.environ.get("TMUX") is not None
        self.debug_log = Path.home() / ".claude" / "tmux-notify-debug.log"

    def log_debug(self, message: str) -> None:
        """Write debug message to log file"""
        try:
            # Ensure directory exists
            self.debug_log.parent.mkdir(parents=True, exist_ok=True)

            with self.debug_log.open("a") as f:
                timestamp = datetime.now(UTC).strftime("%Y-%m-%d %H:%M:%S")
                f.write(f"[{timestamp}] {message}\n")
                f.flush()
        except OSError:
            # If logging fails, don't break the hook
            pass

    def is_pane_visible(self) -> bool:
        """Check if current tmux pane is visible to the user"""
        self.log_debug(f"Checking pane visibility. in_tmux={self.in_tmux}")

        if not self.in_tmux:
            self.log_debug("Not in tmux, assuming visible")
            return True  # If not in tmux, assume visible

        try:
            # Get the pane ID where Claude is running from environment
            current_pane_id = os.environ.get("TMUX_PANE", "")

            if not current_pane_id:
                return True

            # Get the active pane ID
            active_pane_id = subprocess.run(
                ["tmux", "display-message", "-p", "#{pane_id}"],
                capture_output=True,
                text=True,
                check=True,
            ).stdout.strip()
        except (OSError, subprocess.CalledProcessError) as e:
            # If we can't determine state, assume visible to avoid annoying notifications
            self.log_debug(f"Error checking tmux state: {e}")
            return True
        else:
            # Compare pane IDs
            return current_pane_id == active_pane_id

    def notify(self, message: str) -> None:
        """Send notification if pane is not visible"""
        self.log_debug(f"Notify called with message: {message}")

        if self.is_pane_visible():
            # Don't notify if user can see this pane
            self.log_debug("Pane is visible, skipping notification")
            return

        self.log_debug("Pane not visible, sending notification")

        # Ring the terminal bell
        sys.stderr.write("\a")
        sys.stderr.flush()
        self.log_debug("Terminal bell sent")

        # Write bell to the pane's TTY to trigger tmux urgent flag
        if self.in_tmux:
            try:
                current_pane = os.environ.get("TMUX_PANE", "")
                if current_pane:
                    # Get the TTY of the Claude pane
                    pane_tty = subprocess.run(
                        [
                            "tmux",
                            "display-message",
                            "-p",
                            "-t",
                            current_pane,
                            "#{pane_tty}",
                        ],
                        capture_output=True,
                        text=True,
                        check=True,
                    ).stdout.strip()
                    self.log_debug(f"Got pane TTY: {pane_tty}")

                    # Write bell directly to the TTY
                    with Path(pane_tty).open("w") as tty:
                        tty.write("\a")
                        tty.flush()
                    self.log_debug(f"Bell written to TTY {pane_tty} for urgent flag")
            except (OSError, subprocess.CalledProcessError) as e:
                self.log_debug(f"Failed to write bell to TTY: {e}")

        # Display tmux message if in tmux
        if self.in_tmux:
            try:
                subprocess.run(
                    [
                        "tmux",
                        "display-message",
                        "-d",
                        "2000",  # Display for 3 seconds
                        f"⚡ {message}",
                    ],
                    check=True,
                )
                self.log_debug("Tmux display message sent successfully")
            except subprocess.CalledProcessError as e:
                self.log_debug(f"Failed to send tmux display message: {e}")


def main() -> None:
    # Initialize notifier first for logging
    notifier = ClaudeTmuxNotifier()
    notifier.log_debug("=== Hook script started ===")

    # Read JSON input from stdin
    try:
        input_data = sys.stdin.read()
        notifier.log_debug(f"Input data received: {input_data!r}")

        if input_data:
            hook_data = json.loads(input_data)
        else:
            hook_data = {}
            notifier.log_debug("No input data received")
    except json.JSONDecodeError as e:
        # If JSON parsing fails, exit gracefully
        notifier.log_debug(f"JSON decode error: {e}")
        sys.exit(0)

    # Check if this is a simple notification message
    if (
        "message" in hook_data
        and "needs your permission" in hook_data.get("message", "").lower()
    ):
        notifier.log_debug("Detected Notification event")
        # Use the full message from the hook data
        full_message = hook_data.get("message", "Claude needs permission")
        notifier.notify(full_message)
    else:
        notifier.log_debug("no message found in hook data, skipping notification")


if __name__ == "__main__":
    main()
