#!/usr/bin/env python3
"""URL parsing and validation functions for buildbot-pr-check."""

import re
import urllib.parse
from dataclasses import dataclass

from .exceptions import InvalidPRURLError


@dataclass
class BuildInfo:
    """Information extracted from a buildbot URL"""

    base_url: str | None
    builder_id: str | None
    build_num: str | None


def is_safe_url(url: str) -> bool:
    """Check if URL is safe to open (only https allowed)"""
    try:
        parsed = urllib.parse.urlparse(url)
        return parsed.scheme == "https" and bool(parsed.netloc)
    except (ValueError, AttributeError):
        return False


def get_pr_info(pr_url: str) -> tuple[str, str, str, str]:
    """Extract platform, owner, repo, and PR number from PR URL"""
    # GitHub pattern
    match = re.match(r"https://github.com/([^/]+)/([^/]+)/pull/(\d+)", pr_url)
    if match:
        return "github", match.group(1), match.group(2), match.group(3)

    # Gitea pattern (e.g., https://git.clan.lol/clan/clan-core/pulls/4210)
    match = re.match(r"https://([^/]+)/([^/]+)/([^/]+)/pulls/(\d+)", pr_url)
    if match:
        return "gitea", match.group(2), match.group(3), match.group(4)

    raise InvalidPRURLError(f"Invalid PR URL: {pr_url}. Supported: GitHub and Gitea")


def extract_build_info(buildbot_url: str) -> BuildInfo:
    """Extract builder and build number from buildbot URL"""
    # Pattern: https://buildbot.dse.in.tum.de/#/builders/18/builds/710
    match = re.search(r"/builders/(\d+)/builds/(\d+)", buildbot_url)
    if match:
        base_url = buildbot_url.split("/")[2]
        return BuildInfo(
            base_url=base_url, builder_id=match.group(1), build_num=match.group(2)
        )
    return BuildInfo(base_url=None, builder_id=None, build_num=None)
