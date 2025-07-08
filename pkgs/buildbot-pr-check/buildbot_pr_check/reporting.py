#!/usr/bin/env python3
"""Reporting functions for buildbot-pr-check."""

import logging
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass

from .build_status import BuildStatus
from .buildbot_api import (
    BuildWithTriggers,
    check_build_request_status,
    get_build_log_urls,
    get_build_names,
    get_parent_build_status,
)
from .colors import Colors, colorize

logger = logging.getLogger(__name__)


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
