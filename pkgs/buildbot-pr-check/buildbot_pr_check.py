#!/usr/bin/env python3
import argparse
import json
import logging
import os
import re
import subprocess
import sys
import urllib.error
import urllib.parse
import urllib.request
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from enum import IntEnum

# Set up logging
logger = logging.getLogger(__name__)


# Build status enum
class BuildStatus(IntEnum):
    """Build status codes used by Buildbot"""

    SUCCESS = 0
    WARNINGS = 1
    FAILURE = 2
    SKIPPED = 3
    EXCEPTION = 4
    RETRY = 5
    CANCELLED = 6

    @property
    def display_name(self) -> str:
        """Get display name for the status"""
        return self.name

    @property
    def icon(self) -> str:
        """Get icon for the status"""
        icons = {
            BuildStatus.SUCCESS: "✅",
            BuildStatus.WARNINGS: "⚠️",
            BuildStatus.FAILURE: "❌",
            BuildStatus.SKIPPED: "⏭️",
            BuildStatus.EXCEPTION: "💥",
            BuildStatus.RETRY: "🔄",
            BuildStatus.CANCELLED: "⚠️",
        }
        return icons.get(self, "•")

    @property
    def color(self) -> str:
        """Get color for the status"""
        colors = {
            BuildStatus.SUCCESS: Colors.GREEN,
            BuildStatus.WARNINGS: Colors.YELLOW,
            BuildStatus.FAILURE: Colors.RED,
            BuildStatus.SKIPPED: Colors.CYAN,
            BuildStatus.EXCEPTION: Colors.RED,
            BuildStatus.RETRY: Colors.YELLOW,
            BuildStatus.CANCELLED: Colors.YELLOW,
        }
        return colors.get(self, Colors.RESET)

    @property
    def title(self) -> str:
        """Get title for the status"""
        titles = {
            BuildStatus.SUCCESS: "Successful builds",
            BuildStatus.WARNINGS: "Builds with warnings",
            BuildStatus.FAILURE: "Failed builds",
            BuildStatus.SKIPPED: "Skipped builds",
            BuildStatus.EXCEPTION: "Builds with exceptions",
            BuildStatus.RETRY: "Retried builds",
            BuildStatus.CANCELLED: "Canceled builds",
        }
        return titles.get(self, f"{self.display_name} builds")


# Color codes for terminal output
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


@dataclass
class BuildInfo:
    """Information extracted from a buildbot URL"""

    base_url: str | None
    builder_id: str | None
    build_num: str | None


@dataclass
class BuildRequestStatus:
    """Status information for a build request"""

    request_id: int
    status: BuildStatus | None
    build_id: int | None
    virtual_builder_name: str | None = None


@dataclass
class BuildWithTriggers:
    """A build that has triggered sub-builds"""

    url: str
    base_url: str
    builder_id: str
    build_num: str
    build_requests: list[int]


@dataclass
class LogUrl:
    """Log URL information"""

    step_name: str
    log_name: str
    url: str


class BuildbotCheckError(Exception):
    """Base exception for buildbot-pr-check errors"""

    pass


class InvalidPRURLError(BuildbotCheckError):
    """Raised when PR URL is invalid or unsupported"""

    pass


class APIError(BuildbotCheckError):
    """Raised when API calls fail"""

    pass


class BuildbotAPIError(APIError):
    """Raised when Buildbot API calls fail"""

    pass


class GitHubAPIError(APIError):
    """Raised when GitHub API calls fail"""

    pass


class GiteaAPIError(APIError):
    """Raised when Gitea API calls fail"""

    pass


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


def get_triggered_builds(base_url: str, builder_id: str, build_num: str) -> list[int]:
    """Get all triggered builds from a parent build"""
    # Get build steps
    api_url = (
        f"https://{base_url}/api/v2/builders/{builder_id}/builds/{build_num}/steps"
    )

    build_requests = []

    try:
        with urllib.request.urlopen(api_url) as response:  # noqa: S310
            data = json.loads(response.read())
    except (urllib.error.URLError, urllib.error.HTTPError) as e:
        raise BuildbotAPIError(f"Failed to fetch build steps from Buildbot: {e}")
    except json.JSONDecodeError as e:
        raise BuildbotAPIError(f"Failed to parse Buildbot API response: {e}")

    for step in data.get("steps", []):
        if step.get("name") == "build flake" or "build" in step.get("name", "").lower():
            # Extract build request IDs from URLs
            for url_info in step.get("urls", []):
                url = url_info.get("url", "")
                if "buildrequests" in url:
                    match = re.search(r"buildrequests/(\d+)", url)
                    if match:
                        build_requests.append(int(match.group(1)))

    return sorted(build_requests)


def get_build_status(result_code: int | None) -> BuildStatus | None:
    """Convert Buildbot result code to BuildStatus enum."""
    if result_code is None:
        return None
    try:
        return BuildStatus(result_code)
    except ValueError:
        # Unknown status code
        return None


def check_build_request_status(base_url: str, request_id: int) -> BuildRequestStatus:
    """Check the status of a build request and return build ID"""
    api_url = f"https://{base_url}/api/v2/buildrequests/{request_id}?property=*"

    try:
        with urllib.request.urlopen(api_url) as response:  # noqa: S310
            data = json.loads(response.read())
            request = data["buildrequests"][0]
            result = request.get("results")
            status = get_build_status(result)

            # Get virtual_builder_name from properties
            virtual_builder_name = None
            properties = request.get("properties", {})
            if properties and "virtual_builder_name" in properties:
                virtual_builder_name = properties["virtual_builder_name"][0]

            # Get build ID from the builds endpoint
            build_id = None
            if request.get("complete"):
                builds_url = (
                    f"https://{base_url}/api/v2/buildrequests/{request_id}/builds"
                )
                try:
                    with urllib.request.urlopen(builds_url) as builds_response:  # noqa: S310
                        builds_data = json.loads(builds_response.read())
                        if builds_data.get("builds"):
                            build_id = builds_data["builds"][0].get("buildid")
                except (
                    urllib.error.URLError,
                    urllib.error.HTTPError,
                    json.JSONDecodeError,
                ):
                    pass

            return BuildRequestStatus(
                request_id=request_id,
                status=status,
                build_id=build_id,
                virtual_builder_name=virtual_builder_name,
            )
    except (
        urllib.error.URLError,
        urllib.error.HTTPError,
        json.JSONDecodeError,
        KeyError,
    ):
        return BuildRequestStatus(request_id=request_id, status=None, build_id=None)


def get_step_log_urls(base_url: str, step_id: int, step_name: str) -> list[LogUrl]:
    """Get log URLs for a specific step."""
    log_urls = []
    logs_url = f"https://{base_url}/api/v2/steps/{step_id}/logs"

    try:
        with urllib.request.urlopen(logs_url) as log_response:  # noqa: S310
            log_data = json.loads(log_response.read())

            for log in log_data.get("logs", []):
                log_id = log.get("logid")
                if log_id:
                    raw_log_url = f"https://{base_url}/api/v2/logs/{log_id}/raw_inline"
                    log_urls.append(
                        LogUrl(
                            step_name=step_name,
                            log_name=log.get("name", "stdio"),
                            url=raw_log_url,
                        )
                    )
    except (urllib.error.URLError, urllib.error.HTTPError, json.JSONDecodeError):
        pass

    return log_urls


def get_build_log_urls(base_url: str, build_id: int) -> list[LogUrl]:
    """Get log URLs for a build"""
    if build_id is None:
        return []

    log_urls = []

    try:
        # Get build steps
        steps_url = f"https://{base_url}/api/v2/builds/{build_id}/steps"
        with urllib.request.urlopen(steps_url) as response:  # noqa: S310
            data = json.loads(response.read())
            steps = data.get("steps", [])

            if not steps:
                return []

            # Use ThreadPoolExecutor for parallel log fetching
            max_workers = min(10, max(1, len(steps)))  # Limit concurrent connections
            with ThreadPoolExecutor(max_workers=max_workers) as executor:
                # Submit all step log requests to the thread pool
                future_to_step = {}
                for step in steps:
                    step_id = step.get("stepid")
                    if step_id:
                        step_name = step.get("name", "Unknown step")
                        future = executor.submit(
                            get_step_log_urls, base_url, step_id, step_name
                        )
                        future_to_step[future] = (step_id, step_name)

                # Collect results as they complete
                for future in as_completed(future_to_step):
                    try:
                        step_logs = future.result()
                        log_urls.extend(step_logs)
                    except Exception as e:
                        step_id, step_name = future_to_step[future]
                        print(
                            f"Error getting logs for step {step_name} (ID: {step_id}): {e}"
                        )

    except (urllib.error.URLError, urllib.error.HTTPError, json.JSONDecodeError):
        return []

    return log_urls


def get_parent_build_status(
    base_url: str, builder_id: str, build_num: str
) -> tuple[BuildStatus | None, list[LogUrl]]:
    """Get parent build status and logs from failed steps."""
    try:
        # Get build data
        build_url = (
            f"https://{base_url}/api/v2/builders/{builder_id}/builds/{build_num}"
        )
        with urllib.request.urlopen(build_url) as response:  # noqa: S310
            data = json.loads(response.read())
            build = data["builds"][0]

            # Check if build failed
            results = build.get("results")
            if results is None:
                return None, []  # Build still in progress

            status = get_build_status(results)

            # If failed, get logs from failed steps
            log_urls = []
            if status in [
                BuildStatus.FAILURE,
                BuildStatus.EXCEPTION,
                BuildStatus.CANCELLED,
            ]:
                steps_url = f"https://{base_url}/api/v2/builders/{builder_id}/builds/{build_num}/steps"
                with urllib.request.urlopen(steps_url) as steps_response:  # noqa: S310
                    steps_data = json.loads(steps_response.read())

                    for step in steps_data.get("steps", []):
                        # Check if step failed
                        step_results = step.get("results")
                        if step_results and step_results >= 2:  # FAILURE or worse
                            step_id = step.get("stepid")
                            if step_id:
                                step_logs = get_step_log_urls(
                                    base_url, step_id, step.get("name", "Unknown step")
                                )
                                log_urls.extend(step_logs)

            return status, log_urls

    except (urllib.error.URLError, urllib.error.HTTPError, json.JSONDecodeError) as e:
        print(f"Warning: Could not fetch parent build status: {e}")
        return None, []


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


def filter_builds_with_triggers(buildbot_urls: list[str]) -> list[BuildWithTriggers]:
    """Filter buildbot URLs to only include those with triggered sub-builds."""
    builds_with_triggers = []

    for url in buildbot_urls:
        build_info = extract_build_info(url)
        if build_info.base_url and build_info.builder_id and build_info.build_num:
            try:
                build_requests = get_triggered_builds(
                    build_info.base_url, build_info.builder_id, build_info.build_num
                )
                # Include builds even without triggered builds so we can check parent build status
                builds_with_triggers.append(
                    BuildWithTriggers(
                        url=url,
                        base_url=build_info.base_url,
                        builder_id=build_info.builder_id,
                        build_num=build_info.build_num,
                        build_requests=build_requests,
                    )
                )
            except BuildbotAPIError as e:
                print(f"Warning: Could not fetch triggered builds for {url}: {e}")
                continue

    return builds_with_triggers


@dataclass
class BuildStatusReport:
    """Status report for a build"""

    statuses: dict[BuildStatus | None, list[int]]
    build_id_map: dict[int, int | None]
    name_map: dict[int, str]
    virtual_builder_map: dict[int, str | None]


def check_build_status(build: BuildWithTriggers) -> BuildStatusReport:
    """Check status of all build requests for a build."""
    print(f"\n{colorize('🔍 Checking:', Colors.CYAN)} {build.url}")
    print("─" * 80)

    # First check parent build status
    parent_status, parent_logs = get_parent_build_status(
        build.base_url, build.builder_id, build.build_num
    )

    if parent_status and parent_status in [
        BuildStatus.FAILURE,
        BuildStatus.EXCEPTION,
        BuildStatus.CANCELLED,
    ]:
        print(
            f"{colorize('⚠️  Parent build failed:', Colors.RED)} {parent_status.display_name}"
        )
        if parent_logs:
            print(f"\n{colorize('📋 Parent build logs:', Colors.CYAN)}")
            for log in parent_logs:
                print(
                    f"  • {log.step_name} ({log.log_name}): {colorize(log.url, Colors.BLUE)}"
                )
        print()

    print(
        f"Found {colorize(str(len(build.build_requests)), Colors.BOLD)} triggered builds"
    )

    # Get build names mapping
    name_map = get_build_names(build.base_url, build.builder_id, build.build_num)

    # Check status of each build request in parallel
    statuses: dict[BuildStatus | None, list[int]] = {}
    build_id_map = {}
    virtual_builder_map = {}

    # Use ThreadPoolExecutor for parallel requests
    max_workers = min(
        20, max(1, len(build.build_requests))
    )  # Limit concurrent connections
    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        # Submit all requests to the thread pool
        future_to_req_id = {
            executor.submit(check_build_request_status, build.base_url, req_id): req_id
            for req_id in build.build_requests
        }

        # Process results as they complete
        for future in as_completed(future_to_req_id):
            req_id = future_to_req_id[future]
            try:
                req_status = future.result()
                if req_status.status not in statuses:
                    statuses[req_status.status] = []
                statuses[req_status.status].append(req_id)
                build_id_map[req_id] = req_status.build_id
                virtual_builder_map[req_id] = req_status.virtual_builder_name
            except Exception as e:
                # Handle any errors from the thread
                print(f"Error checking request {req_id}: {e}")
                if None not in statuses:
                    statuses[None] = []
                statuses[None].append(req_id)
                build_id_map[req_id] = None
                virtual_builder_map[req_id] = None

    return BuildStatusReport(
        statuses=statuses,
        build_id_map=build_id_map,
        name_map=name_map,
        virtual_builder_map=virtual_builder_map,
    )


def print_build_report(
    build: BuildWithTriggers,
    report: BuildStatusReport,
    included_statuses: set[BuildStatus] | None = None,
) -> None:
    """Print detailed report for a build.

    Args:
        build: The build with triggers
        report: The build status report
        included_statuses: Set of statuses to include in detailed output. If None, defaults to FAILURE and CANCELLED.
    """
    # Default to showing FAILURE and CANCELLED if not specified
    if included_statuses is None:
        included_statuses = {BuildStatus.FAILURE, BuildStatus.CANCELLED}

    # Report summary
    print(f"\n{colorize('📊 Build Summary:', Colors.BOLD)}")
    for status, requests in sorted(
        report.statuses.items(), key=lambda x: (x[0] is None, x[0].value if x[0] else 0)
    ):
        if status is None:
            icon = "•"
            status_colored = "ERROR"
        else:
            icon = status.icon
            status_colored = colorize(status.display_name, status.color)
        print(f"  {icon} {status_colored}: {len(requests)} builds")

    # Show detailed output for included statuses
    for status in included_statuses:
        if status not in report.statuses:
            continue

        print(
            f"\n{colorize(f'{status.icon} {status.title}', status.color)} ({len(report.statuses[status])} total):"
        )
        for req_id in sorted(report.statuses[status]):
            # Use virtual_builder_name if available, otherwise fall back to name_map
            virtual_name = report.virtual_builder_map.get(req_id)
            if virtual_name:
                # Extract just the flake attribute part
                if "#" in virtual_name:
                    display_name = virtual_name.split("#", 1)[1]
                else:
                    display_name = virtual_name
            else:
                display_name = report.name_map.get(req_id, f"Request {req_id}")

            print(f"  → {colorize(display_name, status.color)}")

            # Get log URLs for failed builds
            if status not in [
                BuildStatus.FAILURE,
                BuildStatus.EXCEPTION,
                BuildStatus.CANCELLED,
            ]:
                continue

            build_id = report.build_id_map.get(req_id)
            if not build_id:
                logger.debug(f"No build_id found for request {req_id}")
                continue

            logger.debug(f"Fetching log URLs for build_id {build_id}")
            log_urls = get_build_log_urls(build.base_url, build_id)
            if not log_urls:
                logger.debug(f"No log URLs found for build_id {build_id}")
                continue

            print(f"    {colorize('Log URLs:', Colors.CYAN)}")
            for log in log_urls:
                print(
                    f"      • {log.step_name} ({log.log_name}): {colorize(log.url, Colors.BLUE)}"
                )

    # Handle None status (errors) if present
    if None not in report.statuses or None not in included_statuses:
        return

    print(
        f"\n{colorize('• ERROR builds', Colors.RED)} ({len(report.statuses[None])} total):"
    )
    for req_id in sorted(report.statuses[None]):
        display_name = report.name_map.get(req_id, f"Request {req_id}")
        print(f"  → {colorize(display_name, Colors.RED)}")


def check_pr(pr_url: str, included_statuses: set[BuildStatus] | None = None) -> int:
    """Check buildbot status for a pull request.

    Args:
        pr_url: The GitHub or Gitea pull request URL
        included_statuses: Set of statuses to include in detailed output

    Returns:
        Exit code: 0 for success, 1 for failure/canceled builds
    """
    try:
        platform, owner, repo, pr_num = get_pr_info(pr_url)
        print(
            f"{colorize('🔎 Checking PR', Colors.BOLD)} #{colorize(pr_num, Colors.CYAN)} in {colorize(f'{owner}/{repo}', Colors.BLUE)} ({platform})"
        )
        print("═" * 80)

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

        print(
            f"Found {colorize(str(len(buildbot_urls)), Colors.BOLD)} buildbot build(s)"
        )

        # Filter out builds without triggered builds
        builds_with_triggers = filter_builds_with_triggers(buildbot_urls)

        if not builds_with_triggers:
            print(
                f"\n{colorize('No buildbot builds with triggered sub-builds found', Colors.YELLOW)}"
            )
            print(
                "All CI statuses appear to be for builds without triggered sub-builds"
            )
            sys.exit(0)

        print(
            f"\nFound {colorize(str(len(builds_with_triggers)), Colors.BOLD)} build(s) with triggered sub-builds"
        )

        # Process each build
        exit_code = 0
        for build in builds_with_triggers:
            report = check_build_status(build)
            print_build_report(build, report, included_statuses)

            # Check parent build status for exit code
            parent_status, _ = get_parent_build_status(
                build.base_url, build.builder_id, build.build_num
            )
            if parent_status in [
                BuildStatus.FAILURE,
                BuildStatus.EXCEPTION,
                BuildStatus.CANCELLED,
            ]:
                exit_code = 1

            # Set exit code to 1 if there are any failures or cancellations
            if (
                BuildStatus.FAILURE in report.statuses
                or BuildStatus.CANCELLED in report.statuses
            ):
                exit_code = 1

        return exit_code

    except BuildbotCheckError as e:
        print(f"Error: {e}")
        return 1


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


def parse_included_statuses(value: str) -> set[BuildStatus]:
    """Parse comma-separated list of status names."""
    # Split by comma and convert to uppercase
    status_names = {s.strip().upper() for s in value.split(",") if s.strip()}

    # Convert to BuildStatus enum
    statuses = set()
    valid_names = {status.name for status in BuildStatus}

    for name in status_names:
        if name in valid_names:
            statuses.add(BuildStatus[name])
        else:
            raise argparse.ArgumentTypeError(
                f"Invalid status name: {name}. Valid statuses: {', '.join(sorted(valid_names))}"
            )

    return statuses


def main() -> None:
    """Main entry point for command-line usage."""
    parser = argparse.ArgumentParser(
        description="Check buildbot status for GitHub/Gitea pull requests",
        epilog="""
Examples:
  GitHub: buildbot-pr-check https://github.com/TUM-DSE/doctor-cluster-config/pull/459
  Gitea:  buildbot-pr-check https://git.clan.lol/clan/clan-core/pulls/4210
  Auto:   buildbot-pr-check  # Uses current branch
  Show skipped: buildbot-pr-check --include SKIPPED,SUCCESS

Optional: Set GITHUB_TOKEN environment variable for API rate limits
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument(
        "pr_url",
        nargs="?",
        help="Pull request URL (GitHub or Gitea). If not provided, will try to detect PR for current branch",
    )

    parser.add_argument(
        "--include",
        type=parse_included_statuses,
        help=f"Comma-separated list of statuses to show details for. Default: FAILURE,CANCELLED. Valid values: {', '.join(status.name for status in BuildStatus)}",
    )

    parser.add_argument("--debug", action="store_true", help="Enable debug logging")

    args = parser.parse_args()

    # Configure logging
    if args.debug:
        logging.basicConfig(level=logging.DEBUG, format="%(levelname)s: %(message)s")
    else:
        logging.basicConfig(level=logging.WARNING, format="%(levelname)s: %(message)s")

    # Get PR URL
    pr_url = args.pr_url
    if pr_url is None:
        pr_url = get_current_branch_pr_url()
        if not pr_url:
            print(
                "Error: No PR URL provided and could not detect PR for current branch"
            )
            print("\nMake sure you have:")
            print("  1. An open PR for the current branch")
            print("  2. The 'gh' CLI tool installed and authenticated")
            print("\nOr provide the PR URL explicitly:")
            print("  buildbot-pr-check <pr-url>")
            sys.exit(1)
        print(f"Auto-detected PR: {pr_url}")

    exit_code = check_pr(pr_url, args.include)
    sys.exit(exit_code)


if __name__ == "__main__":
    main()
