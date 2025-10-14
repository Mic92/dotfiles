#!/usr/bin/env python3
"""
merge-when-green - Create PR and merge when CI passes
"""

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.request
from enum import Enum
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


class Platform(Enum):
    """Git hosting platform."""

    GITHUB = "github"
    GITEA = "gitea"


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


def run(
    cmd: list[str], check: bool = True, capture: bool = False
) -> subprocess.CompletedProcess[str]:
    """Run a command."""
    if capture:
        result = subprocess.run(cmd, check=False, capture_output=True, text=True)
        if result.returncode != 0 and check:
            raise subprocess.CalledProcessError(result.returncode, cmd)
        return result
    return subprocess.run(cmd, check=check, text=True)


def detect_platform() -> Platform:
    """Detect platform: GitHub or Gitea."""
    # Try GitHub first
    result = run(["gh", "repo", "view", "--json", "name"], check=False, capture=True)
    if result.returncode == 0:
        print_subtle("Detected GitHub")
        return Platform.GITHUB

    # Try Gitea
    result = run(["tea", "repos", "list", "--limit", "1"], check=False, capture=True)
    if result.returncode == 0:
        print_subtle("Detected Gitea")
        return Platform.GITEA

    print_warning("Could not detect platform, defaulting to GitHub")
    return Platform.GITHUB


def get_default_branch(platform: Platform) -> str:
    """Get default branch."""
    if platform == Platform.GITHUB:
        result = run(
            [
                "gh",
                "repo",
                "view",
                "--json",
                "defaultBranchRef",
                "--jq",
                ".defaultBranchRef.name",
            ],
            capture=True,
        )
        return result.stdout.strip()

    # Gitea: use git symbolic-ref
    result = run(
        ["git", "symbolic-ref", "refs/remotes/origin/HEAD"], check=False, capture=True
    )
    if result.returncode == 0:
        return result.stdout.strip().split("/")[-1]
    return "main"


def get_repo_info() -> tuple[str, str, str]:
    """Parse git remote to get API URL, owner, repo."""
    result = run(["git", "remote", "get-url", "origin"], capture=True)
    remote_url = result.stdout.strip()

    # SSH: git@host:owner/repo.git or HTTPS: https://host/owner/repo.git
    match = re.match(
        r"(?:https?://|git@)([^/:]+)[:/]([^/]+)/(.+?)(?:\.git)?$", remote_url
    )
    if not match:
        msg = f"Could not parse remote URL: {remote_url}"
        raise RuntimeError(msg)

    host, owner, repo = match.groups()
    api_url = f"https://{host}"
    return api_url, owner, repo


def check_pr_exists(branch: str, platform: Platform) -> bool:
    """Check if a PR already exists for this branch."""
    if platform == Platform.GITHUB:
        result = run(
            ["gh", "pr", "view", branch, "--json", "state"],
            check=False,
            capture=True,
        )
        if result.returncode == 0:
            try:
                pr_data = json.loads(result.stdout)
                state = pr_data.get("state")
                return bool(state == "OPEN")
            except json.JSONDecodeError:
                pass
    else:
        # Gitea
        result = run(
            ["tea", "pulls", "list", "--output", "json", "--state", "open"],
            check=False,
            capture=True,
        )
        if result.returncode == 0:
            try:
                prs = json.loads(result.stdout)
                for pr in prs:
                    if pr.get("head", {}).get("ref") == branch:
                        return True
            except json.JSONDecodeError:
                pass
    return False


def create_pr_github(branch: str, target: str, title: str, body: str) -> str:
    """Create GitHub PR and enable auto-merge."""
    result = run(
        [
            "gh",
            "pr",
            "create",
            "--title",
            title,
            "--body",
            body,
            "--base",
            target,
            "--head",
            branch,
        ],
        check=False,
    )
    if result.returncode != 0:
        print_warning("PR creation failed, likely already exists")

    print_warning("Enabling auto-merge...")
    run(["gh", "pr", "merge", branch, "--auto", "--rebase"])
    print_success("✓ Auto-merge enabled")
    return branch


def create_pr_gitea(branch: str, target: str, title: str, body: str) -> str:
    """Create Gitea PR and enable server-side auto-merge."""
    # Create PR
    result = run(
        [
            "tea",
            "pulls",
            "create",
            "--head",
            branch,
            "--base",
            target,
            "--title",
            title,
            "--description",
            body,
            "--output",
            "json",
        ],
        capture=True,
    )

    try:
        pr_data = json.loads(result.stdout)
        pr_index = str(pr_data["index"])
    except (json.JSONDecodeError, KeyError):
        print_warning("Could not parse PR number, using branch name")
        return branch

    # Enable auto-merge via API
    print_warning("Enabling auto-merge...")
    api_url, owner, repo = get_repo_info()
    token = os.environ.get("GITEA_TOKEN")

    url = f"{api_url}/api/v1/repos/{owner}/{repo}/pulls/{pr_index}/merge"
    headers = {"Content-Type": "application/json"}
    if token:
        headers["Authorization"] = f"token {token}"

    data = json.dumps(
        {
            "Do": "merge",
            "merge_when_checks_succeed": True,
            "delete_branch_after_merge": True,
        }
    ).encode()

    req = urllib.request.Request(url, data=data, headers=headers, method="POST")  # noqa: S310
    try:
        urllib.request.urlopen(req, timeout=10)  # noqa: S310
        print_success("✓ Auto-merge enabled")
    except (urllib.error.HTTPError, urllib.error.URLError) as e:
        print_warning(f"Could not enable auto-merge: {e}")

    return pr_index


def check_github_pr_state(pr_id: str) -> bool | None:
    """Check GitHub PR state. Returns True if merged, False if closed, None if open."""
    result = run(
        ["gh", "pr", "view", pr_id, "--json", "state"], check=False, capture=True
    )
    if result.returncode != 0:
        return None
    pr_data = json.loads(result.stdout)
    state = pr_data.get("state", "")
    if state == "MERGED":
        return True
    if state == "CLOSED":
        print_error("PR was closed")
        return False
    return None


def check_gitea_pr_state(pr_id: str) -> bool | None:
    """Check Gitea PR state. Returns True if merged, False if closed, None if open."""
    result = run(
        ["tea", "pulls", "list", "--output", "json", "--state", "all"],
        check=False,
        capture=True,
    )
    if result.returncode != 0:
        return None

    try:
        prs = json.loads(result.stdout)
        for pr in prs:
            if str(pr.get("index")) == pr_id:
                state = pr.get("state", "").lower()
                if state == "closed":
                    if pr.get("merged"):
                        return True
                    print_error("PR was closed without merging")
                    return False
                break
    except json.JSONDecodeError:
        pass
    return None


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


def get_pr_status_github(pr_id: str) -> tuple[dict[str, Any] | None, str]:
    """Get PR status from GitHub."""
    result = run(
        [
            "gh",
            "pr",
            "view",
            pr_id,
            "--json",
            "state,mergeable,autoMergeRequest,statusCheckRollup,url",
        ],
        check=False,
        capture=True,
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
            run(["buildbot-pr-check", pr_url], check=False)
            print()  # Add blank line after buildbot-pr-check output
        return True
    return buildbot_check_done


def wait_for_merge(platform: Platform, pr_id: str) -> bool:
    """Wait for PR to be merged."""
    print_header(f"Waiting for PR '{pr_id}' to merge...")

    if platform == Platform.GITEA:
        # Gitea: simple polling
        while True:
            result = check_gitea_pr_state(pr_id)
            if result is not None:
                return result
            print(f"[{time.strftime('%H:%M:%S')}] Waiting...")
            time.sleep(30)

    # GitHub: detailed check monitoring
    buildbot_check_done = False
    while True:
        pr_data, error = get_pr_status_github(pr_id)
        if pr_data is None:
            print_error(error)
            return False

        checks = pr_data.get("statusCheckRollup", [])
        pending, failed, passed = count_check_states(checks)

        # Print status
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
        completion = check_pr_completion(pr_data, pending, failed)
        if completion is not None:
            success, message = completion
            if not success:
                print_error(f"\n✗ {message}")
            return success

        # Still waiting
        time.sleep(10)


def get_pr_message_from_editor(default_branch: str) -> tuple[str, str]:
    """Get PR title/body by opening editor with commit messages."""
    remote = (
        "upstream"
        if "upstream" in run(["git", "remote"], capture=True).stdout
        else "origin"
    )
    commits = run(
        [
            "git",
            "log",
            "--reverse",
            "--pretty=format:%s%n%n%b%n%n",
            f"{remote}/{default_branch}..HEAD",
        ],
        capture=True,
    ).stdout

    with tempfile.NamedTemporaryFile(
        mode="w+", suffix="_COMMIT_EDITMSG", delete=False
    ) as f:
        f.write(commits)
        f.flush()
        editor = os.environ.get("EDITOR", "vim")
        subprocess.run([editor, f.name], check=True)
        f.seek(0)
        msg = f.read()
    Path(f.name).unlink()

    lines = msg.split("\n", 1)
    return lines[0], lines[1] if len(lines) > 1 else ""


def prepare_repository(default_branch: str) -> int:
    """Prepare repository: pull, format check. Returns 0 if ready, 1 on error."""
    print_header("Preparing changes...")
    run(["git", "pull", "--rebase", "origin", default_branch])

    print_header("Checking code formatting...")
    result = run(["flake-fmt"], check=False)
    if result.returncode != 0:
        print_warning("Formatting issues found. Attempting to fix...")
        run(
            [
                "git",
                "absorb",
                "--force",
                "--and-rebase",
                "--base",
                f"origin/{default_branch}",
            ],
            check=False,
        )
        if sys.stdin.isatty() and sys.stdout.isatty():
            run(["lazygit"], check=False)
        else:
            print_error("Formatting check failed. Please run 'flake-fmt' manually.")
        return 1

    result = run(["git", "diff", "--quiet", f"origin/{default_branch}"], check=False)
    if result.returncode == 0:
        print_success("✓ No changes to merge")
        return 1
    return 0


def get_pr_message(message_arg: str | None, default_branch: str) -> tuple[str, str]:
    """Get PR title and body from args or editor."""
    if message_arg:
        lines = message_arg.split("\n", 1)
        title = lines[0]
        body = lines[1] if len(lines) > 1 else ""
        return title, body
    return get_pr_message_from_editor(default_branch)


def push_branch(branch_name: str, default_branch: str) -> str:
    """Push branch and return the branch name to use for PR."""
    current_branch = run(
        ["git", "branch", "--show-current"], capture=True
    ).stdout.strip()

    if current_branch == default_branch:
        branch_name = f"merge-when-green-{os.environ.get('USER', 'user')}"
    else:
        branch_name = current_branch

    print_header("Pushing changes...")
    run(["git", "push", "--force", "origin", f"HEAD:{branch_name}"])
    return branch_name


def enable_automerge_existing_pr(branch_name: str, platform: Platform) -> str:
    """Enable auto-merge on existing PR. Returns PR ID."""
    print_warning("Enabling auto-merge...")
    if platform == Platform.GITHUB:
        run(["gh", "pr", "merge", branch_name, "--auto", "--rebase"])
        print_success("✓ Auto-merge enabled")
        return branch_name

    # Gitea: need to get the PR number first
    result = run(
        ["tea", "pulls", "list", "--output", "json", "--state", "open"],
        capture=True,
    )
    try:
        prs = json.loads(result.stdout)
        for pr in prs:
            if pr.get("head", {}).get("ref") == branch_name:
                pr_id = str(pr["index"])
                # Enable auto-merge via API
                api_url, owner, repo = get_repo_info()
                token = os.environ.get("GITEA_TOKEN")
                url = f"{api_url}/api/v1/repos/{owner}/{repo}/pulls/{pr_id}/merge"
                headers = {"Content-Type": "application/json"}
                if token:
                    headers["Authorization"] = f"token {token}"
                data = json.dumps(
                    {
                        "Do": "merge",
                        "merge_when_checks_succeed": True,
                        "delete_branch_after_merge": True,
                    }
                ).encode()
                req = urllib.request.Request(  # noqa: S310
                    url, data=data, headers=headers, method="POST"
                )
                try:
                    urllib.request.urlopen(req, timeout=10)  # noqa: S310
                    print_success("✓ Auto-merge enabled")
                except (urllib.error.HTTPError, urllib.error.URLError) as e:
                    print_warning(f"Could not enable auto-merge: {e}")
                return pr_id
    except json.JSONDecodeError:
        print_warning("Could not parse PR list")
    return branch_name


def finalize_merge(platform: Platform, pr_id: str, default_branch: str) -> int:
    """Wait for merge and rebase. Returns exit code."""
    if wait_for_merge(platform, pr_id):
        print_success("\n✓ PR merged!")
        run(["git", "fetch", "origin", default_branch])
        run(["git", "rebase", f"origin/{default_branch}"])
        print_success("✓ Rebased onto latest changes")
        return 0
    return 1


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(description="Create PR and merge when CI passes")
    parser.add_argument(
        "--no-wait", action="store_true", help="Don't wait for CI checks to complete"
    )
    parser.add_argument(
        "-m", "--message", help="PR title and body (separated by newline)"
    )
    args = parser.parse_args()

    platform = detect_platform()

    print_header("Getting repository information...")
    default_branch = get_default_branch(platform)
    print_info(f"Target branch: {Colors.BLUE}{default_branch}{Colors.RESET}")

    if prepare_repository(default_branch) != 0:
        return 1

    branch_name = push_branch("", default_branch)

    # Check if PR already exists
    if check_pr_exists(branch_name, platform):
        print_success("✓ Using existing pull request")
        pr_id = enable_automerge_existing_pr(branch_name, platform)
    else:
        title, body = get_pr_message(args.message, default_branch)
        print_header("Creating pull request...")
        if platform == Platform.GITHUB:
            pr_id = create_pr_github(branch_name, default_branch, title, body)
        else:
            pr_id = create_pr_gitea(branch_name, default_branch, title, body)
        print_success("✓ Pull request created")

    if not args.no_wait:
        return finalize_merge(platform, pr_id, default_branch)

    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except KeyboardInterrupt:
        print_warning("\nInterrupted")
        sys.exit(130)
    except subprocess.CalledProcessError as e:
        sys.exit(e.returncode)
