#!/usr/bin/env python3
"""GitHub API functions for buildbot-pr-check."""

import json
import logging
import os
import subprocess
import urllib.error
import urllib.request

from .exceptions import GitHubAPIError
from .url_parser import is_safe_url

logger = logging.getLogger(__name__)


def get_github_token() -> str | None:
    """Get GitHub token from gh CLI or environment"""
    # First check environment variable
    github_token = os.environ.get("GITHUB_TOKEN")
    if github_token:
        return github_token

    # Try to get token from gh CLI
    try:
        result = subprocess.run(
            ["gh", "auth", "token"], capture_output=True, text=True, check=True
        )
        token = result.stdout.strip()
        if token:
            return token
    except subprocess.CalledProcessError as e:
        logger.debug(f"gh CLI not authenticated: {e}")
    except FileNotFoundError:
        logger.debug(
            "gh CLI not found, falling back to GITHUB_TOKEN environment variable"
        )

    return None


def get_buildbot_urls_from_github(owner: str, repo: str, pr_num: str) -> list[str]:
    """Get buildbot URLs from GitHub PR checks"""
    # Get PR commits
    api_url = f"https://api.github.com/repos/{owner}/{repo}/pulls/{pr_num}"

    req = urllib.request.Request(api_url)  # noqa: S310
    req.add_header("Accept", "application/vnd.github.v3+json")

    # Get GitHub token
    github_token = get_github_token()
    if github_token:
        req.add_header("Authorization", f"token {github_token}")

    try:
        with urllib.request.urlopen(req) as response:  # noqa: S310
            pr_data = json.loads(response.read())
            head_sha = pr_data["head"]["sha"]
    except (urllib.error.URLError, urllib.error.HTTPError) as e:
        raise GitHubAPIError(f"Failed to fetch PR data from GitHub: {e}")
    except json.JSONDecodeError as e:
        raise GitHubAPIError(f"Failed to parse GitHub API response: {e}")

    # Get check runs for the commit
    checks_url = (
        f"https://api.github.com/repos/{owner}/{repo}/commits/{head_sha}/check-runs"
    )
    req = urllib.request.Request(checks_url)  # noqa: S310
    req.add_header("Accept", "application/vnd.github.v3+json")
    if github_token:
        req.add_header("Authorization", f"token {github_token}")

    buildbot_urls = []

    try:
        with urllib.request.urlopen(req) as response:  # noqa: S310
            checks_data = json.loads(response.read())

            for check in checks_data.get("check_runs", []):
                # Look for buildbot checks
                if (
                    "buildbot" in check.get("name", "").lower()
                    or "buildbot" in check.get("app", {}).get("name", "").lower()
                ):
                    details_url = check.get("details_url", "")
                    if "buildbot" in details_url and is_safe_url(details_url):
                        buildbot_urls.append(details_url)
    except (urllib.error.URLError, urllib.error.HTTPError, json.JSONDecodeError):
        pass

    # Also check commit statuses
    status_url = (
        f"https://api.github.com/repos/{owner}/{repo}/commits/{head_sha}/status"
    )
    req = urllib.request.Request(status_url)  # noqa: S310
    req.add_header("Accept", "application/vnd.github.v3+json")
    if github_token:
        req.add_header("Authorization", f"token {github_token}")

    try:
        with urllib.request.urlopen(req) as response:  # noqa: S310
            status_data = json.loads(response.read())

            for status in status_data.get("statuses", []):
                if "buildbot" in status.get("context", "").lower():
                    target_url = status.get("target_url", "")
                    if (
                        target_url
                        and "buildbot" in target_url
                        and is_safe_url(target_url)
                    ):
                        buildbot_urls.append(target_url)
    except (urllib.error.URLError, urllib.error.HTTPError, json.JSONDecodeError):
        pass

    return list(set(buildbot_urls))  # Remove duplicates
