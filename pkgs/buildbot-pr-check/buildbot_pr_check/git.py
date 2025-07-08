#!/usr/bin/env python3
"""Git utilities for buildbot-pr-check."""

import json
import subprocess


def get_current_branch_pr_url() -> str | None:
    """Try to get the PR URL for the current branch using gh CLI."""
    try:
        # Get current branch name
        result = subprocess.run(
            ["git", "branch", "--show-current"],
            capture_output=True,
            text=True,
            check=True,
        )
        branch = result.stdout.strip()

        if not branch:
            return None

        # Try to get PR info for current branch
        result = subprocess.run(
            ["gh", "pr", "view", branch, "--json", "url"],
            capture_output=True,
            text=True,
            check=False,
        )

        if result.returncode == 0:
            try:
                data = json.loads(result.stdout)
                return data.get("url")
            except json.JSONDecodeError:
                pass

    except (subprocess.CalledProcessError, FileNotFoundError):
        pass

    return None
