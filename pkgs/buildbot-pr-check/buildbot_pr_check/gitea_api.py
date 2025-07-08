#!/usr/bin/env python3
"""Gitea API functions for buildbot-pr-check."""

import json
import re
import urllib.error
import urllib.request

from .url_parser import is_safe_url


def get_buildbot_urls_from_gitea(
    pr_url: str, owner: str, repo: str, pr_num: str
) -> list[str]:
    """Get buildbot URLs from Gitea PR"""
    # Extract base URL from PR URL
    match = re.match(r"(https://[^/]+)", pr_url)
    if not match:
        return []

    base_url = match.group(1)
    api_url = f"{base_url}/api/v1/repos/{owner}/{repo}/pulls/{pr_num}"

    buildbot_urls = []

    try:
        req = urllib.request.Request(api_url)  # noqa: S310
        req.add_header("Accept", "application/json")

        with urllib.request.urlopen(req) as response:  # noqa: S310
            pr_data = json.loads(response.read())
            head_sha = pr_data.get("head", {}).get("sha", "")

            if head_sha:
                # Get commit status
                status_url = (
                    f"{base_url}/api/v1/repos/{owner}/{repo}/statuses/{head_sha}"
                )
                req = urllib.request.Request(status_url)  # noqa: S310
                req.add_header("Accept", "application/json")

                try:
                    with urllib.request.urlopen(req) as status_response:  # noqa: S310
                        statuses = json.loads(status_response.read())

                        for status in statuses:
                            if "buildbot" in status.get("context", "").lower():
                                target_url = status.get("target_url", "")
                                if (
                                    target_url
                                    and "buildbot" in target_url
                                    and is_safe_url(target_url)
                                ):
                                    buildbot_urls.append(target_url)
                except (
                    urllib.error.URLError,
                    urllib.error.HTTPError,
                    json.JSONDecodeError,
                ):
                    pass
    except (urllib.error.URLError, urllib.error.HTTPError, json.JSONDecodeError):
        pass

    return list(set(buildbot_urls))
