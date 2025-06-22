"""Import ClaudeSpamFilter from the afew_filters package."""
# ruff: noqa: INP001

import sys
from pathlib import Path

# Add the script directory to Python path so afew_filters can be imported
sys.path.insert(0, str(Path(__file__).parent))

from afew_filters.claude_spam_filter import ClaudeSpamFilter

__all__ = ["ClaudeSpamFilter"]
