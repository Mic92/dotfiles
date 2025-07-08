#!/usr/bin/env python3
"""Command-line interface for buildbot-pr-check."""

import argparse
import logging
import sys

from .build_status import BuildStatus
from .buildbot_api import filter_builds_with_triggers, get_parent_build_status
from .colors import Colors, colorize
from .exceptions import BuildbotCheckError
from .git import get_current_branch_pr_url
from .gitea_api import get_buildbot_urls_from_gitea
from .github_api import get_buildbot_urls_from_github
from .reporting import check_build_status, print_build_report
from .url_parser import get_pr_info

logger = logging.getLogger(__name__)


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
