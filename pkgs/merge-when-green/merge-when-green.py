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
    BLUE = "\033[94m"
    GREEN = "\033[92m"
    YELLOW = "\033[93m"
    RED = "\033[91m"
    GRAY = "\033[90m"
    BOLD = "\033[1m"
    RESET = "\033[0m"


class Platform(Enum):
    GITHUB = "github"
    GITEA = "gitea"


def print_success(message: str) -> None:
    print(f"{Colors.GREEN}{message}{Colors.RESET}")


def print_error(message: str) -> None:
    print(f"{Colors.RED}{message}{Colors.RESET}")


def print_warning(message: str) -> None:
    print(f"{Colors.YELLOW}{message}{Colors.RESET}")


def print_header(message: str) -> None:
    print(f"\n{Colors.BOLD}{message}{Colors.RESET}")


def print_subtle(message: str) -> None:
    print(f"{Colors.GRAY}{message}{Colors.RESET}")


def run(
    cmd: list[str], check: bool = True, capture: bool = False
) -> subprocess.CompletedProcess[str]:
    if capture:
        result = subprocess.run(cmd, check=False, capture_output=True, text=True)
        if result.returncode != 0 and check:
            raise subprocess.CalledProcessError(result.returncode, cmd)
        return result
    return subprocess.run(cmd, check=check, text=True)


def detect_platform() -> Platform:
    result = run(["gh", "repo", "view", "--json", "name"], check=False, capture=True)
    if result.returncode == 0:
        print_subtle("Detected GitHub")
        return Platform.GITHUB

    result = run(["tea", "repos", "list", "--limit", "1"], check=False, capture=True)
    if result.returncode == 0:
        print_subtle("Detected Gitea")
        return Platform.GITEA

    print_warning("Could not detect platform, defaulting to GitHub")
    return Platform.GITHUB


def get_default_branch(platform: Platform) -> str:
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


def gitea_enable_automerge(pr_index: str) -> None:
    """Gitea has no CLI support for auto-merge, so use its REST API."""
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


def create_pr_gitea(branch: str, target: str, title: str, body: str) -> str:
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

    gitea_enable_automerge(pr_index)
    return pr_index


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
    pr_data: dict[str, Any], pending: int, failed: int, *, in_merge_queue: bool
) -> tuple[bool, str] | None:
    """Check if PR has reached a completion state. Returns None if still waiting."""
    state = pr_data.get("state", "UNKNOWN")
    mergeable = pr_data.get("mergeable", "UNKNOWN")
    auto_merge = pr_data.get("autoMergeRequest") is not None

    if state == "MERGED":
        return True, "PR successfully merged!"

    if state == "CLOSED":
        return False, "PR was closed"

    # Once a PR enters the merge queue GitHub clears autoMergeRequest, so only
    # treat a missing autoMergeRequest as "disabled" when the PR is not queued.
    if not auto_merge and not in_merge_queue:
        return False, "Auto-merge was disabled"

    if mergeable == "CONFLICTING":
        return False, "PR has merge conflicts"

    if failed > 0 and pending == 0:
        return False, f"{failed} checks failed"

    return None  # still waiting


def get_pr_status_github(pr_id: str) -> tuple[dict[str, Any] | None, str]:
    result = run(
        [
            "gh",
            "pr",
            "view",
            pr_id,
            "--json",
            "number,state,mergeable,autoMergeRequest,statusCheckRollup,url",
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


def get_merge_queue_status_github(pr_number: int) -> tuple[bool, str | None]:
    """Query merge-queue membership via GraphQL.

    `gh pr view --json` does not expose isInMergeQueue, so we need a direct
    GraphQL call. Returns (is_in_queue, human_readable_position_or_None).
    """
    repo = run(
        ["gh", "repo", "view", "--json", "owner,name"], check=False, capture=True
    )
    if repo.returncode != 0:
        return False, None
    try:
        repo_data = json.loads(repo.stdout)
        owner = repo_data["owner"]["login"]
        name = repo_data["name"]
    except (json.JSONDecodeError, KeyError):
        return False, None

    query = """
        query($owner: String!, $name: String!, $number: Int!) {
          repository(owner: $owner, name: $name) {
            pullRequest(number: $number) {
              isInMergeQueue
              mergeQueueEntry { state position }
            }
          }
        }
    """
    result = run(
        [
            "gh",
            "api",
            "graphql",
            "-f",
            f"query={query}",
            "-F",
            f"owner={owner}",
            "-F",
            f"name={name}",
            "-F",
            f"number={pr_number}",
        ],
        check=False,
        capture=True,
    )
    if result.returncode != 0:
        return False, None
    try:
        data = json.loads(result.stdout)["data"]["repository"]["pullRequest"]
    except (json.JSONDecodeError, KeyError, TypeError):
        return False, None

    in_queue = bool(data.get("isInMergeQueue"))
    entry = data.get("mergeQueueEntry") or {}
    desc = None
    if in_queue and entry:
        state = entry.get("state", "")
        pos = entry.get("position")
        desc = f"queue pos {pos}, {state}" if pos is not None else state
    return in_queue, desc


def run_nixbot_log_if_needed(failed: int, pending: int, nixbot_log_done: bool) -> bool:
    """Show CI failure logs via nbo (nixbot's CLI) once all checks finished."""
    if failed > 0 and pending == 0 and not nixbot_log_done:
        if shutil.which("nbo"):
            print_warning("\nRunning nbo log to get failure details...")
            run(["nbo", "log"], check=False)
            print()
        return True
    return nixbot_log_done


def wait_for_merge(platform: Platform, pr_id: str) -> bool:
    print_header(f"Waiting for PR '{pr_id}' to merge...")

    if platform == Platform.GITEA:
        while True:
            result = check_gitea_pr_state(pr_id)
            if result is not None:
                return result
            print(f"[{time.strftime('%H:%M:%S')}] Waiting...")
            time.sleep(30)

    nixbot_log_done = False
    while True:
        pr_data, error = get_pr_status_github(pr_id)
        if pr_data is None:
            print_error(error)
            return False

        checks = pr_data.get("statusCheckRollup", [])
        pending, failed, passed = count_check_states(checks)

        in_merge_queue, queue_desc = get_merge_queue_status_github(
            int(pr_data.get("number", 0))
        )

        queue_suffix = (
            f" {Colors.BLUE}[merge queue: {queue_desc}]{Colors.RESET}"
            if in_merge_queue
            else ""
        )
        print(
            f"[{time.strftime('%H:%M:%S')}] "
            f"Checks - {Colors.GREEN}Passed: {passed}{Colors.RESET}, "
            f"{Colors.RED}Failed: {failed}{Colors.RESET}, "
            f"{Colors.YELLOW}Pending: {pending}{Colors.RESET}"
            f"{queue_suffix}"
        )

        nixbot_log_done = run_nixbot_log_if_needed(failed, pending, nixbot_log_done)

        completion = check_pr_completion(
            pr_data, pending, failed, in_merge_queue=in_merge_queue
        )
        if completion is not None:
            success, message = completion
            if not success:
                print_error(f"\n✗ {message}")
            return success

        time.sleep(10)


def get_pr_message_from_editor(default_branch: str) -> tuple[str, str]:
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
    """Pull and format-check. Returns 0 if there is something to merge."""
    print_header("Preparing changes...")
    # submodule.recurse=true makes `pull --rebase` return 128 when the current
    # branch introduces a new submodule that the base branch doesn't have yet —
    # the recursive submodule checkout sees the initialized submodule as a
    # "local modification" and refuses. The rebase itself is fine; only the
    # recursive bit trips. Disable it for this call and sync submodules after.
    run(
        [
            "git",
            "-c",
            "submodule.recurse=false",
            "pull",
            "--rebase",
            "origin",
            default_branch,
        ]
    )
    run(["git", "submodule", "update", "--init", "--recursive"], check=False)

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
    if message_arg:
        lines = message_arg.split("\n", 1)
        return lines[0], lines[1] if len(lines) > 1 else ""
    return get_pr_message_from_editor(default_branch)


def push_branch(default_branch: str) -> str:
    """Push HEAD and return the branch name to use for the PR."""
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
    """Enable auto-merge on an existing PR. Returns the PR ID."""
    if platform == Platform.GITHUB:
        print_warning("Enabling auto-merge...")
        run(["gh", "pr", "merge", branch_name, "--auto", "--rebase"])
        print_success("✓ Auto-merge enabled")
        return branch_name

    # Gitea addresses PRs by number, so look it up from the branch name
    result = run(
        ["tea", "pulls", "list", "--output", "json", "--state", "open"],
        capture=True,
    )
    try:
        prs = json.loads(result.stdout)
        for pr in prs:
            if pr.get("head", {}).get("ref") == branch_name:
                pr_id = str(pr["index"])
                gitea_enable_automerge(pr_id)
                return pr_id
    except json.JSONDecodeError:
        print_warning("Could not parse PR list")
    return branch_name


def finalize_merge(platform: Platform, pr_id: str, default_branch: str) -> int:
    if wait_for_merge(platform, pr_id):
        print_success("\n✓ PR merged!")
        run(["git", "fetch", "origin", default_branch])
        run(["git", "rebase", f"origin/{default_branch}"])
        print_success("✓ Rebased onto latest changes")
        return 0
    return 1


def chdir_repo_root() -> None:
    """Change to the git repository root so all commands run from there."""
    result = run(["git", "rev-parse", "--show-toplevel"], check=False, capture=True)
    if result.returncode != 0:
        print_error("Not inside a git repository")
        raise SystemExit(1)
    os.chdir(result.stdout.strip())


def main() -> int:
    parser = argparse.ArgumentParser(description="Create PR and merge when CI passes")
    parser.add_argument(
        "--no-wait", action="store_true", help="Don't wait for CI checks to complete"
    )
    parser.add_argument(
        "-m", "--message", help="PR title and body (separated by newline)"
    )
    args = parser.parse_args()

    chdir_repo_root()

    platform = detect_platform()

    print_header("Getting repository information...")
    default_branch = get_default_branch(platform)
    print(f"Target branch: {Colors.BLUE}{default_branch}{Colors.RESET}")

    if prepare_repository(default_branch) != 0:
        return 1

    branch_name = push_branch(default_branch)

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
