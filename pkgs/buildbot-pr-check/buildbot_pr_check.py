#!/usr/bin/env python3
import json
import os
import re
import subprocess
import sys
import traceback
import urllib.error
import urllib.parse
import urllib.request


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

    msg = f"Invalid PR URL: {pr_url}. Supported: GitHub and Gitea"
    raise ValueError(msg)


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
    except (subprocess.CalledProcessError, FileNotFoundError):
        # gh CLI not available or not authenticated
        pass

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

    with urllib.request.urlopen(req) as response:  # noqa: S310
        pr_data = json.loads(response.read())
        head_sha = pr_data["head"]["sha"]

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


def extract_build_info(buildbot_url: str) -> tuple[str, str, str]:
    """Extract builder and build number from buildbot URL"""
    # Pattern: https://buildbot.dse.in.tum.de/#/builders/18/builds/710
    match = re.search(r"/builders/(\d+)/builds/(\d+)", buildbot_url)
    if match:
        return buildbot_url.split("/")[2], match.group(1), match.group(2)
    return None, None, None


def get_triggered_builds(base_url: str, builder_id: str, build_num: str) -> list[int]:
    """Get all triggered builds from a parent build"""
    # Get build steps
    api_url = (
        f"https://{base_url}/api/v2/builders/{builder_id}/builds/{build_num}/steps"
    )

    build_requests = []

    with urllib.request.urlopen(api_url) as response:  # noqa: S310
        data = json.loads(response.read())

        for step in data.get("steps", []):
            if (
                step.get("name") == "build flake"
                or "build" in step.get("name", "").lower()
            ):
                # Extract build request IDs from URLs
                for url_info in step.get("urls", []):
                    url = url_info.get("url", "")
                    if "buildrequests" in url:
                        match = re.search(r"buildrequests/(\d+)", url)
                        if match:
                            build_requests.append(int(match.group(1)))

    return sorted(build_requests)


def check_build_request_status(
    base_url: str, request_id: int
) -> tuple[str, str, int | None]:
    """Check the status of a build request and return build ID"""
    api_url = f"https://{base_url}/api/v2/buildrequests/{request_id}"

    result_meanings = {
        0: "SUCCESS",
        1: "WARNINGS",
        2: "FAILURE",
        3: "SKIPPED",
        4: "EXCEPTION",
        5: "RETRY",
        6: "CANCELLED",
    }

    try:
        with urllib.request.urlopen(api_url) as response:  # noqa: S310
            data = json.loads(response.read())
            request = data["buildrequests"][0]
            result = request.get("results")
            status = result_meanings.get(result, f"UNKNOWN({result})")
            build_id = request.get("buildid")
            return request_id, status, build_id
    except (
        urllib.error.URLError,
        urllib.error.HTTPError,
        json.JSONDecodeError,
        KeyError,
    ):
        return request_id, "ERROR", None


def get_build_log_urls(base_url: str, build_id: int) -> list[dict[str, str]]:
    """Get log URLs for a build"""
    if build_id is None:
        return []

    log_urls = []

    try:
        # Get build steps
        steps_url = f"https://{base_url}/api/v2/builds/{build_id}/steps"
        with urllib.request.urlopen(steps_url) as response:  # noqa: S310
            data = json.loads(response.read())

            for step in data.get("steps", []):
                # Get logs for each step
                step_id = step.get("stepid")
                if step_id:
                    logs_url = f"https://{base_url}/api/v2/steps/{step_id}/logs"
                    try:
                        with urllib.request.urlopen(logs_url) as log_response:  # noqa: S310
                            log_data = json.loads(log_response.read())

                            for log in log_data.get("logs", []):
                                log_id = log.get("logid")
                                if log_id:
                                    raw_log_url = f"https://{base_url}/api/v2/logs/{log_id}/raw_inline"
                                    log_urls.append(
                                        {
                                            "step_name": step.get(
                                                "name", "Unknown step"
                                            ),
                                            "log_name": log.get("name", "stdio"),
                                            "url": raw_log_url,
                                        }
                                    )
                    except (
                        urllib.error.URLError,
                        urllib.error.HTTPError,
                        json.JSONDecodeError,
                    ):
                        pass
    except (urllib.error.URLError, urllib.error.HTTPError, json.JSONDecodeError):
        pass

    return log_urls


def get_build_names(base_url: str, builder_id: str, build_num: str) -> dict[int, str]:
    """Get mapping of request IDs to build names"""
    api_url = (
        f"https://{base_url}/api/v2/builders/{builder_id}/builds/{build_num}/properties"
    )

    request_to_name = {}

    try:
        with urllib.request.urlopen(api_url) as response:  # noqa: S310
            data = json.loads(response.read())
            props = data["properties"][0]

            # Extract all nixos systems in order
            systems = []
            for key in sorted(props.keys()):
                if ".nixos-" in key and "-drv_path" in key:
                    system_name = key.split(".nixos-")[1].split("-drv_path")[0]
                    arch = key.split(".")[0]
                    systems.append(f"{arch}.nixos-{system_name}")

            # Map to request IDs (assuming they start from the first triggered request)
            if systems:
                # We need to find the starting request ID
                build_requests = get_triggered_builds(base_url, builder_id, build_num)
                if build_requests:
                    start_id = min(build_requests)
                    for i, system in enumerate(systems):
                        request_to_name[start_id + i] = system
    except (urllib.error.URLError, urllib.error.HTTPError, json.JSONDecodeError):
        pass

    return request_to_name


def check_pr(pr_url: str) -> int:
    """Check buildbot status for a pull request.

    Args:
        pr_url: The GitHub or Gitea pull request URL

    Returns:
        Exit code: 0 for success, 1 for failure/canceled builds
    """
    try:
        platform, owner, repo, pr_num = get_pr_info(pr_url)
        print(f"Checking PR #{pr_num} in {owner}/{repo} ({platform})")
        print("=" * 80)

        # Get buildbot URLs from PR
        if platform == "github":
            buildbot_urls = get_buildbot_urls_from_github(owner, repo, pr_num)
        else:  # gitea
            buildbot_urls = get_buildbot_urls_from_gitea(pr_url, owner, repo, pr_num)

        if not buildbot_urls:
            print("No buildbot builds found for this PR")
            print("\nTrying to find builds manually...")
            print("Please check the PR page for buildbot links in:")
            print("  - Check runs")
            print("  - Status checks")
            print("  - PR comments")
            sys.exit(0)

        print(f"Found {len(buildbot_urls)} buildbot build(s)")

        for url in buildbot_urls:
            print(f"\nChecking: {url}")
            print("-" * 80)

            base_url, builder_id, build_num = extract_build_info(url)
            if not base_url:
                print("Could not parse buildbot URL")
                continue

            # Get triggered builds
            build_requests = get_triggered_builds(base_url, builder_id, build_num)

            if not build_requests:
                print("No triggered builds found")
                continue

            print(f"Found {len(build_requests)} triggered builds")

            # Get build names mapping
            name_map = get_build_names(base_url, builder_id, build_num)

            # Check status of each build request
            statuses: dict[str, list[int]] = {}
            build_info = {}  # Store build IDs for each request
            for req_id in build_requests:
                _, status, build_id = check_build_request_status(base_url, req_id)
                if status not in statuses:
                    statuses[status] = []
                statuses[status].append(req_id)
                build_info[req_id] = build_id

            # Report summary
            print("\nBuild Summary:")
            for status, requests in sorted(statuses.items()):
                print(f"  {status}: {len(requests)} builds")

            # Show canceled builds with names
            if "CANCELLED" in statuses:
                print(f"\nCanceled builds ({len(statuses['CANCELLED'])} total):")
                for req_id in sorted(statuses["CANCELLED"]):
                    name = name_map.get(req_id, f"Request {req_id}")
                    print(f"  - {name}")

            # Show failed builds with names and log URLs
            if "FAILURE" in statuses:
                print(f"\nFailed builds ({len(statuses['FAILURE'])} total):")
                for req_id in sorted(statuses["FAILURE"]):
                    name = name_map.get(req_id, f"Request {req_id}")
                    print(f"  - {name}")

                    # Get log URLs for this failed build
                    build_id = build_info.get(req_id)
                    if build_id:
                        log_urls = get_build_log_urls(base_url, build_id)
                        if log_urls:
                            print("    Log URLs:")
                            for log_info in log_urls:
                                print(
                                    f"      • {log_info['step_name']} ({log_info['log_name']}): {log_info['url']}"
                                )

        # Final summary
        print("\n" + "=" * 80)
        print("OVERALL SUMMARY")
        print("=" * 80)

        total_canceled = 0
        total_failed = 0
        total_success = 0

        for url in buildbot_urls:
            base_url, builder_id, build_num = extract_build_info(url)
            if base_url:
                build_requests = get_triggered_builds(base_url, builder_id, build_num)
                if build_requests:
                    for req_id in build_requests:
                        _, status, _ = check_build_request_status(base_url, req_id)
                        if status == "CANCELLED":
                            total_canceled += 1
                        elif status == "FAILURE":
                            total_failed += 1
                        elif status == "SUCCESS":
                            total_success += 1

        if total_canceled > 0 or total_failed > 0:
            print("❌ Issues found:")
            if total_canceled > 0:
                print(f"   - {total_canceled} builds were canceled")
            if total_failed > 0:
                print(f"   - {total_failed} builds failed")
            return 1
        else:
            print(f"✅ All {total_success} builds passed successfully!")
            return 0

    except (
        ValueError,
        urllib.error.URLError,
        urllib.error.HTTPError,
        json.JSONDecodeError,
        KeyError,
        AttributeError,
    ) as e:
        print(f"Error: {e}")
        traceback.print_exc()
        return 1


def main() -> None:
    """Main entry point for command-line usage."""
    if len(sys.argv) != 2:
        print("Usage: python buildbot_pr_check.py <pr-url>")
        print("Examples:")
        print(
            "  GitHub: python buildbot_pr_check.py https://github.com/TUM-DSE/doctor-cluster-config/pull/459"
        )
        print(
            "  Gitea:  python buildbot_pr_check.py https://git.clan.lol/clan/clan-core/pulls/4210"
        )
        print("\nOptional: Set GITHUB_TOKEN environment variable for API rate limits")
        sys.exit(1)

    pr_url = sys.argv[1]
    exit_code = check_pr(pr_url)
    sys.exit(exit_code)


if __name__ == "__main__":
    main()
