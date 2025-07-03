#!/usr/bin/env python3
"""
Claude Code Hook for bell notifications
Rings bell when Claude needs user input
"""

import json
import sys


def main() -> None:
    # Read JSON input from stdin
    try:
        input_data = sys.stdin.read()
        if input_data:
            hook_data = json.loads(input_data)

            # Check if Claude needs permission
            if (
                "message" in hook_data
                and "needs your permission" in hook_data.get("message", "").lower()
            ):
                # Ring the terminal bell
                sys.stderr.write("\a")
                sys.stderr.flush()
    except json.JSONDecodeError:
        pass


if __name__ == "__main__":
    main()
