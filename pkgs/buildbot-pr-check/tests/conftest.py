"""Pytest configuration for buildbot-pr-check tests."""

import os
import sys
from pathlib import Path

# Add parent directory to Python path so we can import the module
sys.path.insert(0, str(Path(__file__).parent.parent))


def pytest_configure(config):
    """Configure pytest."""
    # Set environment variable to disable any potential GitHub token usage during tests
    # unless explicitly testing with tokens
    if "GITHUB_TOKEN" in os.environ and not os.environ.get("PYTEST_USE_REAL_TOKEN"):
        del os.environ["GITHUB_TOKEN"]
