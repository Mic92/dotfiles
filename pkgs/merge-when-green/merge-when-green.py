#!/usr/bin/env python3
"""
merge-when-green - Create PR and merge when CI passes
"""

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.request
from enum import Enum
from pathlib import Path


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


def create_pr_github(branch: str, target: str, title: str, body: str) -> str:
    """Create GitHub PR and enable auto-merge."""
    run(
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
        ]
    )
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


def wait_for_merge(platform: Platform, pr_id: str) -> bool:
    """Wait for PR to be merged."""
    print_header(f"Waiting for PR '{pr_id}' to merge...")

    while True:
        if platform == Platform.GITHUB:
            result = check_github_pr_state(pr_id)
        else:
            result = check_gitea_pr_state(pr_id)

        if result is not None:
            return result

        print(f"[{time.strftime('%H:%M:%S')}] Waiting...")
        time.sleep(30)


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
