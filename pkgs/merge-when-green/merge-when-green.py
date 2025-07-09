#!/usr/bin/env python3
"""
merge-when-green - Create PR and merge when CI passes
"""

import argparse
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Any


class Colors:
    """ANSI color codes for terminal output."""

    BLUE = "\033[94m"
    GREEN = "\033[92m"
    YELLOW = "\033[93m"
    RED = "\033[91m"
    GRAY = "\033[90m"
    BOLD = "\033[1m"
    RESET = "\033[0m"


def print_info(message: str) -> None:
    """Print an informational message."""
    print(message)


def print_success(message: str) -> None:
    """Print a success message in green."""
    print(f"{Colors.GREEN}{message}{Colors.RESET}")


def print_error(message: str) -> None:
    """Print an error message in red."""
    print(f"{Colors.RED}{message}{Colors.RESET}")


def print_warning(message: str) -> None:
    """Print a warning message in yellow."""
    print(f"{Colors.YELLOW}{message}{Colors.RESET}")


def print_header(message: str) -> None:
    """Print a header message in bold."""
    print(f"\n{Colors.BOLD}{message}{Colors.RESET}")


def print_subtle(message: str) -> None:
    """Print a subtle message in gray."""
    print(f"{Colors.GRAY}{message}{Colors.RESET}")


def print_command(cmd_str: str) -> None:
    """Print a command in blue."""
    print(f"{Colors.BLUE}+ {cmd_str}{Colors.RESET}", file=sys.stderr)


def log_command(cmd: list[str]) -> None:
    """Log a command execution in a nice format."""
    # Format the command for display
    formatted_cmd = []
    for arg in cmd:
        # Quote arguments containing spaces or special characters
        if " " in arg or '"' in arg or "'" in arg or "|" in arg or "&" in arg:
            if '"' in arg:
                formatted_cmd.append(f"'{arg}'")
            else:
                formatted_cmd.append(f'"{arg}"')
        else:
            formatted_cmd.append(arg)

    cmd_str = " ".join(formatted_cmd)
    print_command(cmd_str)


def run_command(
    cmd: list[str],
    check: bool = True,
    capture_stdout: bool = False,
    silent: bool = False,
    cwd: str | None = None,
) -> subprocess.CompletedProcess[str]:
    """Run a command and return the result."""
    if not silent:
        log_command(cmd)

    if capture_stdout:
        result = subprocess.run(
            cmd, check=False, stdout=subprocess.PIPE, text=True, cwd=cwd
        )
        if result.returncode != 0 and check:
            print_error(f"Command failed with exit code {result.returncode}")
            raise subprocess.CalledProcessError(
                result.returncode, cmd, result.stdout, result.stderr
            )
        return result
    return subprocess.run(cmd, check=check, text=True, cwd=cwd)


def get_git_remote() -> str:
    """Get the appropriate git remote (upstream or origin)."""
    result = run_command(["git", "remote"], silent=True, capture_stdout=True)
    remotes = result.stdout.strip().split("\n")
    return "upstream" if "upstream" in remotes else "origin"


def get_default_branch() -> str:
    """Get the default branch name from GitHub."""
    result = run_command(
        [
            "gh",
            "repo",
            "view",
            "--json",
            "defaultBranchRef",
            "--jq",
            ".defaultBranchRef.name",
        ],
        capture_stdout=True,
    )
    return result.stdout.strip()


def has_changes(remote: str, target_branch: str) -> bool:
    """Check if there are any changes to merge."""
    result = run_command(
        ["git", "diff", "--quiet", f"{remote}/{target_branch}"], check=False
    )
    return result.returncode != 0


def run_flake_fmt(target_branch: str) -> bool:
    """Run flake-fmt to check and fix formatting. Returns True if successful or not needed."""
    print_header("Checking code formatting...")

    # Get git root directory
    git_root_result = run_command(
        ["git", "rev-parse", "--show-toplevel"], capture_stdout=True, silent=True
    )
    git_root = git_root_result.stdout.strip()

    # Run flake-fmt from git root - it handles all the logic internally
    result = run_command(["flake-fmt"], check=False, cwd=git_root)
    if result.returncode == 0:
        print_success("✓ Code formatting check passed")
        return True

    # If formatting failed, try to absorb changes
    print_warning("Attempting to fix formatting issues...")
    run_command(
        [
            "git",
            "absorb",
            "--force",
            "--and-rebase",
            "--base",
            f"origin/{target_branch}",
        ],
        check=False,
    )
    # Only open lazygit if we're in an interactive shell
    if sys.stdin.isatty() and sys.stdout.isatty():
        run_command(["lazygit"], check=False)
    else:
        print_error("Formatting issues detected. Please run 'flake-fmt' manually.")
    return False


def create_pr(branch: str, target_branch: str, message: str | None = None) -> None:
    """Create a pull request with commit messages as description."""
    print_header("Creating pull request...")
    remote = get_git_remote()

    # Get commit messages
    result = run_command(
        [
            "git",
            "log",
            "--reverse",
            "--pretty=format:%s%n%n%b%n%n",
            f"{remote}/{target_branch}..HEAD",
        ],
        capture_stdout=True,
    )

    if message:
        # Use provided message
        lines = message.split("\n", 1)
        first_line = lines[0]
        rest = lines[1] if len(lines) > 1 else ""
    else:
        # Open editor for commit message
        with tempfile.NamedTemporaryFile(
            mode="w+", suffix="_COMMIT_EDITMSG", delete=False
        ) as f:
            f.write(result.stdout)
            f.flush()

            editor = os.environ.get("EDITOR", "vim")
            print_warning(f"Opening {editor} to edit PR description...")
            subprocess.run([editor, f.name], check=True)

            f.seek(0)
            msg = f.read()

        Path(f.name).unlink()

        # Parse message
        lines = msg.split("\n", 1)
        first_line = lines[0]
        rest = lines[1] if len(lines) > 1 else ""

    # Create PR
    run_command(
        [
            "gh",
            "pr",
            "create",
            "--title",
            first_line,
            "--body",
            rest,
            "--base",
            target_branch,
            "--head",
            branch,
            "--label",
            "merge-queue",
        ]
    )

    # Enable auto-merge
    print_warning("Enabling auto-merge...")
    run_command(["gh", "pr", "merge", branch, "--auto", "--rebase"])
    print_success("✓ Pull request created and auto-merge enabled")


def get_pr_state(branch: str) -> str | None:
    """Get the state of a PR, returns None if PR doesn't exist."""
    result = run_command(
        ["gh", "pr", "view", "--json", "state", "--template", "{{.state}}", branch],
        check=False,
        silent=True,
        capture_stdout=True,
    )
    if result.returncode == 0:
        return result.stdout.strip()
    return None


def check_pr_completion(
    pr_data: dict[str, Any], pending: int, failed: int
) -> tuple[bool, str] | None:
    """Check if PR has reached a completion state. Returns None if still waiting."""
    state = pr_data.get("state", "UNKNOWN")
    mergeable = pr_data.get("mergeable", "UNKNOWN")
    auto_merge = pr_data.get("autoMergeRequest") is not None

    if state == "MERGED":
        return True, "PR successfully merged!"

    if state == "CLOSED":
        return False, "PR was closed"

    if not auto_merge:
        return False, "Auto-merge was disabled"

    if mergeable == "CONFLICTING":
        return False, "PR has merge conflicts"

    if failed > 0 and pending == 0:
        return False, f"{failed} checks failed"

    return None  # Still waiting


def count_check_states(checks: list[dict[str, Any]]) -> tuple[int, int, int]:
    """Count check states from PR status checks."""
    pending = failed = passed = 0
    for check in checks:
        if check.get("__typename") == "CheckRun":
            status = check.get("status")
            conclusion = check.get("conclusion")
            if status != "COMPLETED":
                pending += 1
            elif conclusion in ["SUCCESS", "NEUTRAL", "SKIPPED"]:
                passed += 1
            else:
                failed += 1
        elif check.get("__typename") == "StatusContext":
            check_state = check.get("state")
            if check_state == "PENDING":
                pending += 1
            elif check_state in ["SUCCESS", "NEUTRAL"]:
                passed += 1
            else:
                failed += 1
    return pending, failed, passed


def get_pr_status(branch: str) -> tuple[dict[str, Any] | None, str]:
    """Get PR status from GitHub."""
    result = run_command(
        [
            "gh",
            "pr",
            "view",
            branch,
            "--json",
            "state,mergeable,autoMergeRequest,statusCheckRollup,url",
        ],
        check=False,
        silent=True,
        capture_stdout=True,
    )

    if result.returncode != 0:
        return None, "Failed to get PR status"

    try:
        pr_data = json.loads(result.stdout)
    except json.JSONDecodeError:
        return None, "Failed to parse PR status"
    else:
        return pr_data, ""


def run_buildbot_check_if_needed(
    pr_data: dict[str, Any], failed: int, pending: int, buildbot_check_done: bool
) -> bool:
    """Run buildbot-pr-check if needed."""
    if failed > 0 and pending == 0 and not buildbot_check_done:
        pr_url = pr_data.get("url", "")
        if pr_url and shutil.which("buildbot-pr-check"):
            print_warning(
                "\n🔍 Running buildbot-pr-check to get detailed failure information..."
            )
            run_command(["buildbot-pr-check", pr_url], check=False)
            print()  # Add blank line after buildbot-pr-check output
        return True
    return buildbot_check_done


def wait_for_pr_completion(branch: str, interval: int = 10) -> tuple[bool, str]:
    """Wait for PR to be merged or reach a final state."""
    print_header(f"Waiting for PR completion on '{branch}'...")
    buildbot_check_done = False

    while True:
        pr_data, error = get_pr_status(branch)
        if pr_data is None:
            return False, error

        checks = pr_data.get("statusCheckRollup", [])
        pending, failed, passed = count_check_states(checks)

        # Print status - only show checks
        print(
            f"[{time.strftime('%H:%M:%S')}] "
            f"Checks - {Colors.GREEN}Passed: {passed}{Colors.RESET}, "
            f"{Colors.RED}Failed: {failed}{Colors.RESET}, "
            f"{Colors.YELLOW}Pending: {pending}{Colors.RESET}"
        )

        # Run buildbot-pr-check if we have failing checks
        buildbot_check_done = run_buildbot_check_if_needed(
            pr_data, failed, pending, buildbot_check_done
        )

        # Check for completion
        result = check_pr_completion(pr_data, pending, failed)
        if result is not None:
            return result

        # Still waiting
        time.sleep(interval)


def setup_and_prepare(target_branch: str) -> int:
    """Run flake-fmt and pull latest changes."""
    # Run flake-fmt
    if not run_flake_fmt(target_branch):
        # flake-fmt made changes and opened lazygit
        return 1

    # Pull latest changes
    print_header("Pulling latest changes...")
    run_command(["git", "pull", "--rebase", "origin", target_branch])
    return 0


def push_changes_to_branch(branch: str) -> tuple[bool, str]:
    """Push changes to branch and get the pushed commit."""
    print_header("Pushing changes...")
    push_result = run_command(
        ["git", "push", "--force", "origin", f"HEAD:{branch}"],
        check=False,
        capture_stdout=True,
    )

    if push_result.returncode != 0:
        return False, push_result.stdout

    # Get the commit we just pushed
    pushed_commit = run_command(
        ["git", "rev-parse", "HEAD"], silent=True, capture_stdout=True
    ).stdout.strip()
    return True, pushed_commit


def wait_for_pr_update(branch: str, pushed_commit: str) -> bool:
    """Wait for PR to be updated with pushed commit."""
    print_info("\nWaiting for PR to be updated with pushed changes...")
    wait_start = time.time()
    while True:
        result = run_command(
            ["gh", "pr", "view", branch, "--json", "headRefOid"],
            check=False,
            silent=True,
            capture_stdout=True,
        )
        if result.returncode == 0:
            try:
                pr_data = json.loads(result.stdout)
                pr_commit = pr_data.get("headRefOid", "")
                if pr_commit == pushed_commit:
                    print_success("✓ PR updated with latest commit")
                    return True
            except json.JSONDecodeError:
                pass

        if time.time() - wait_start > 30:
            print_error("✗ Timeout waiting for PR to update")
            return False

        time.sleep(2)


def handle_pr_creation(
    branch: str, target_branch: str, pr_state: str | None, message: str | None
) -> None:
    """Handle PR creation or update existing PR."""
    if pr_state != "OPEN":
        create_pr(branch, target_branch, message)
    else:
        print_success("\n✓ Using existing PR")
        # Enable auto-merge for existing PR
        print_warning("Enabling auto-merge for existing PR...")
        run_command(["gh", "pr", "merge", branch, "--auto", "--rebase"])
        print_success("✓ Auto-merge enabled")


def handle_wait_and_merge(branch: str, pushed_commit: str, target_branch: str) -> int:
    """Handle waiting for PR and merging process."""
    # Wait for PR update
    if not wait_for_pr_update(branch, pushed_commit):
        return 1

    # Now wait for completion
    success, message = wait_for_pr_completion(branch)
    if success:
        print_success(f"\n✓ {message}")

        # Fetch and rebase after successful merge
        print_header("Fetching and rebasing after merge...")
        try:
            # Fetch the latest changes from origin
            run_command(["git", "fetch", "origin", target_branch])

            # Rebase onto the updated target branch
            run_command(["git", "rebase", f"origin/{target_branch}"])
        except subprocess.CalledProcessError as e:
            print_error(f"Failed to fetch and rebase: {e}")
            print_warning("You may need to manually fetch and rebase")
            return 1
        else:
            print_success("✓ Successfully fetched and rebased onto latest changes")
            return 0
    else:
        print_error(f"\n✗ {message}")
        return 1


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(description="Create PR and merge when CI passes")
    parser.add_argument(
        "--no-wait", action="store_true", help="Don't wait for CI checks to complete"
    )
    parser.add_argument(
        "--message",
        "-m",
        help="PR title and body (separated by newline). If not provided, opens editor.",
    )
    args = parser.parse_args()

    # Get target branch
    print_header("Getting repository information...")
    target_branch = get_default_branch()
    print_info(f"Target branch: {Colors.BLUE}{target_branch}{Colors.RESET}")

    # Setup and prepare
    if setup_and_prepare(target_branch) != 0:
        return 1

    # Check if we have changes
    if not has_changes("origin", target_branch):
        print_success("\n✓ No changes to merge")
        return 0

    # Determine branch name
    username = os.environ.get("USER", "unknown")
    branch = f"merge-when-green-{username}"

    # Check if PR already exists
    pr_state = get_pr_state(branch)
    if pr_state == "OPEN":
        print_warning("\nExisting PR found, checking status...")
        run_command(["gh", "pr", "checks", target_branch], check=False)

    # Push changes
    success, result = push_changes_to_branch(branch)
    if not success:
        print_error(f"Failed to push changes: {result}")
        return 1
    pushed_commit = result

    # Create or update PR
    handle_pr_creation(branch, target_branch, pr_state, args.message)

    # Wait for checks unless --no-wait is specified
    if args.no_wait:
        print_success("\n✓ Done!")
        return 0

    # Handle waiting and merging
    return handle_wait_and_merge(branch, pushed_commit, target_branch)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except KeyboardInterrupt:
        print_warning("\nInterrupted by user")
        sys.exit(130)
    except subprocess.CalledProcessError as e:
        sys.exit(e.returncode)
