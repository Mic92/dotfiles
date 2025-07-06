#!/usr/bin/env python3
"""Script to record VCR cassettes for tests."""

import subprocess
import sys
from pathlib import Path


def main():
    """Record new cassettes."""
    # Run tests in record mode
    print("Recording cassettes for the two pull requests...")
    print("This will make real HTTP requests to GitHub, Gitea, and Buildbot APIs.")
    print()

    result = subprocess.run(
        [sys.executable, "-m", "pytest", "-v", "tests/test_buildbot_pr_check.py"],
        cwd=Path(__file__).parent.parent,
    )

    return result.returncode


if __name__ == "__main__":
    sys.exit(main())
