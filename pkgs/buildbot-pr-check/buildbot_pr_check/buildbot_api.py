#!/usr/bin/env python3
"""Buildbot API functions for buildbot-pr-check."""

import json
import re
import urllib.error
import urllib.request
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass

from .build_status import BuildStatus, get_build_status
from .exceptions import BuildbotAPIError


@dataclass
class BuildRequestStatus:
    """Status information for a build request"""

    request_id: int
    status: BuildStatus | None
    build_id: int | None
    virtual_builder_name: str | None = None


@dataclass
class LogUrl:
    """Log URL information"""

    step_name: str
    log_name: str
    url: str


@dataclass
class BuildWithTriggers:
    """A build that has triggered sub-builds"""

    url: str
    base_url: str
    builder_id: str
    build_num: str
    build_requests: list[int]


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
    from .url_parser import extract_build_info

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
