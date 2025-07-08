#!/usr/bin/env python3
import os
import sys


class Colors:
    """ANSI color codes for terminal output"""

    RED = "\033[91m"
    GREEN = "\033[92m"
    YELLOW = "\033[93m"
    BLUE = "\033[94m"
    CYAN = "\033[96m"
    RESET = "\033[0m"
    BOLD = "\033[1m"


def use_color() -> bool:
    """Check if we should use colored output"""
    # Respect NO_COLOR environment variable
    if os.environ.get("NO_COLOR"):
        return False
    # Check if stdout is a TTY
    return sys.stdout.isatty()


def colorize(text: str, color: str) -> str:
    """Apply color to text if colors are enabled"""
    if use_color():
        return f"{color}{text}{Colors.RESET}"
    return text
