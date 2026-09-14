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
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any

IS_TTY = sys.stdout.isatty()


class Colors:
    BLUE = "\033[94m" if IS_TTY else ""
    GREEN = "\033[92m" if IS_TTY else ""
    YELLOW = "\033[93m" if IS_TTY else ""
    RED = "\033[91m" if IS_TTY else ""
    GRAY = "\033[90m" if IS_TTY else ""
    BOLD = "\033[1m" if IS_TTY else ""
    RESET = "\033[0m" if IS_TTY else ""
    CLEAR_LINE = "\r\033[2K" if IS_TTY else ""


class Platform(Enum):
    GITHUB = "github"
    GITEA = "gitea"


def print_success(message: str) -> None:
    print(f"{Colors.GREEN}{message}{Colors.RESET}")


def print_error(message: str) -> None:
    print(f"{Colors.RED}{message}{Colors.RESET}", file=sys.stderr)


def print_warning(message: str) -> None:
    print(f"{Colors.YELLOW}{message}{Colors.RESET}")


def print_header(message: str) -> None:
    print(f"\n{Colors.BOLD}▶ {message}{Colors.RESET}")


def print_subtle(message: str) -> None:
    print(f"{Colors.GRAY}{message}{Colors.RESET}")


def run(
    cmd: list[str], check: bool = True, capture: bool = False
) -> subprocess.CompletedProcess[str]:
    if capture:
        result = subprocess.run(cmd, check=False, capture_output=True, text=True)
        if result.returncode != 0 and check:
            raise subprocess.CalledProcessError(
                result.returncode, cmd, output=result.stdout, stderr=result.stderr
            )
        return result
    return subprocess.run(cmd, check=check, text=True)


def run_json(cmd: list[str]) -> Any:
    """Run a command and parse its stdout as JSON; None on any failure."""
    result = run(cmd, check=False, capture=True)
    if result.returncode != 0:
        return None
    try:
        return json.loads(result.stdout)
    except json.JSONDecodeError:
        return None


@dataclass
class Repo:
    host: str
    owner: str
    name: str

    @property
    def api_url(self) -> str:
        return f"https://{self.host}"


REMOTE_URL_RE = re.compile(
    r"""^(?:
        (?:ssh|https?|git)://(?:[^@/]+@)?(?P<host1>[^/:]+)(?::\d+)?/  # scheme://[user@]host[:port]/
      | (?:[^@/]+@)?(?P<host2>[^/:]+):                                # [user@]host:
    )(?P<owner>[^/]+)/(?P<repo>.+?)(?:\.git)?/?$""",
    re.VERBOSE,
)


def parse_remote_url(remote_url: str) -> Repo:
    match = REMOTE_URL_RE.match(remote_url)
    if not match:
        msg = f"Could not parse remote URL: {remote_url}"
        raise RuntimeError(msg)
    host = match.group("host1") or match.group("host2")
    return Repo(host, match.group("owner"), match.group("repo"))


def get_repo_info() -> Repo:
    result = run(["git", "remote", "get-url", "origin"], capture=True)
    return parse_remote_url(result.stdout.strip())


def detect_platform(repo: Repo) -> Platform:
    if repo.host == "github.com":
        print_subtle(f"GitHub: {repo.owner}/{repo.name}")
        return Platform.GITHUB

    result = run(["tea", "logins", "list", "-o", "simple"], check=False, capture=True)
    if result.returncode == 0 and repo.host in result.stdout:
        print_subtle(f"Gitea ({repo.host}): {repo.owner}/{repo.name}")
        return Platform.GITEA

    print_warning(
        f"Remote host {repo.host!r} is not github.com and has no tea login, "
        "defaulting to GitHub"
    )
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


def gitea_list_prs(state: str) -> list[dict[str, Any]]:
    prs = run_json(["tea", "pulls", "list", "--output", "json", "--state", state])
    return prs if isinstance(prs, list) else []


def gitea_find_open_pr(branch: str) -> dict[str, Any] | None:
    for pr in gitea_list_prs("open"):
        if pr.get("head", {}).get("ref") == branch:
            return pr
    return None


def github_find_open_pr(branch: str) -> dict[str, Any] | None:
    pr = run_json(["gh", "pr", "view", branch, "--json", "state,url,number"])
    if isinstance(pr, dict) and pr.get("state") == "OPEN":
        return pr
    return None


def github_enable_automerge(branch: str) -> None:
    print_subtle("Enabling auto-merge...")
    run(["gh", "pr", "merge", branch, "--auto", "--rebase"])
    print_success("✓ Auto-merge enabled")


def create_pr_github(branch: str, target: str, title: str, body: str) -> str:
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
        capture=True,
    )
    url = result.stdout.strip().splitlines()[-1] if result.stdout.strip() else ""
    print_success(f"✓ Pull request created: {Colors.BLUE}{url}{Colors.RESET}")
    github_enable_automerge(branch)
    return branch


def gitea_enable_automerge(repo: Repo, pr_index: str) -> None:
    """Gitea has no CLI support for auto-merge, so use its REST API."""
    print_subtle("Enabling auto-merge...")
    token = os.environ.get("GITEA_TOKEN")
    if not token:
        print_warning("GITEA_TOKEN not set; cannot enable auto-merge via API")
        return

    url = f"{repo.api_url}/api/v1/repos/{repo.owner}/{repo.name}/pulls/{pr_index}/merge"
    headers = {"Content-Type": "application/json", "Authorization": f"token {token}"}
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
    except urllib.error.HTTPError as e:
        detail = e.read().decode(errors="replace").strip()
        print_warning(f"Could not enable auto-merge: {e} {detail}")
    except urllib.error.URLError as e:
        print_warning(f"Could not enable auto-merge: {e}")


def create_pr_gitea(repo: Repo, branch: str, target: str, title: str, body: str) -> str:
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

    url = (
        pr_data.get("html_url")
        or f"{repo.api_url}/{repo.owner}/{repo.name}/pulls/{pr_index}"
    )
    print_success(f"✓ Pull request created: {Colors.BLUE}{url}{Colors.RESET}")
    gitea_enable_automerge(repo, pr_index)
    return pr_index


def check_gitea_pr_state(pr_id: str) -> bool | None:
    """Returns True if merged, False if closed, None if open/unknown."""
    for pr in gitea_list_prs("all"):
        if str(pr.get("index")) == pr_id:
            if pr.get("merged"):
                return True
            if pr.get("state", "").lower() == "closed":
                print_error("PR was closed without merging")
                return False
            break
    return None


@dataclass
class CheckSummary:
    passed: int = 0
    failed_names: list[str] = field(default_factory=list)
    pending_names: list[str] = field(default_factory=list)

    @property
    def failed(self) -> int:
        return len(self.failed_names)

    @property
    def pending(self) -> int:
        return len(self.pending_names)

    def render(self) -> str:
        parts = [f"{Colors.GREEN}✓ {self.passed}{Colors.RESET}"]
        if self.failed:
            parts.append(f"{Colors.RED}✗ {self.failed}{Colors.RESET}")
        if self.pending:
            parts.append(f"{Colors.YELLOW}… {self.pending}{Colors.RESET}")
        if self.pending == 1:
            parts.append(f"{Colors.GRAY}({self.pending_names[0]}){Colors.RESET}")
        return "  ".join(parts)


def summarize_checks(checks: list[dict[str, Any]]) -> CheckSummary:
    s = CheckSummary()
    for check in checks:
        if check.get("__typename") == "CheckRun":
            name = check.get("name") or "?"
            workflow = check.get("workflowName")
            if workflow:
                name = f"{workflow} / {name}"
            if check.get("status") != "COMPLETED":
                s.pending_names.append(name)
            elif check.get("conclusion") in ["SUCCESS", "NEUTRAL", "SKIPPED"]:
                s.passed += 1
            else:
                s.failed_names.append(name)
        elif check.get("__typename") == "StatusContext":
            name = check.get("context") or "?"
            state = check.get("state")
            if state in ["PENDING", "EXPECTED"]:
                s.pending_names.append(name)
            elif state == "SUCCESS":
                s.passed += 1
            else:
                s.failed_names.append(name)
    return s


def check_pr_completion(
    pr_data: dict[str, Any],
    checks: CheckSummary,
    *,
    in_merge_queue: bool,
    automerge_seen: bool,
) -> tuple[bool, str] | None:
    """Returns (success, message) once the PR reached a final state, else None."""
    state = pr_data.get("state", "UNKNOWN")
    mergeable = pr_data.get("mergeable", "UNKNOWN")
    auto_merge = pr_data.get("autoMergeRequest") is not None

    if state == "MERGED":
        return True, "PR merged"

    if state == "CLOSED":
        return False, "PR was closed"

    # GitHub clears autoMergeRequest once the PR enters the merge queue, and it
    # may take a poll or two after `gh pr merge --auto` until it shows up. Only
    # treat it as "disabled" after we have seen it enabled at least once.
    if not auto_merge and not in_merge_queue and automerge_seen:
        return False, "Auto-merge was disabled"

    if mergeable == "CONFLICTING":
        return False, "PR has merge conflicts"

    if checks.failed > 0 and checks.pending == 0:
        return False, f"{checks.failed} check(s) failed"

    return None


def get_pr_status_github(pr_id: str) -> dict[str, Any] | None:
    pr = run_json(
        [
            "gh",
            "pr",
            "view",
            pr_id,
            "--json",
            "number,state,mergeable,autoMergeRequest,statusCheckRollup,url",
        ]
    )
    return pr if isinstance(pr, dict) else None


MERGE_QUEUE_QUERY = """
query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      isInMergeQueue
      mergeQueueEntry { state position }
    }
  }
}
"""


def get_merge_queue_status_github(
    repo: Repo, pr_number: int
) -> tuple[bool, str | None]:
    """`gh pr view --json` does not expose isInMergeQueue, hence GraphQL."""
    result = run_json(
        [
            "gh",
            "api",
            "graphql",
            "-f",
            f"query={MERGE_QUEUE_QUERY}",
            "-F",
            f"owner={repo.owner}",
            "-F",
            f"name={repo.name}",
            "-F",
            f"number={pr_number}",
        ]
    )
    try:
        data = result["data"]["repository"]["pullRequest"]
    except (KeyError, TypeError):
        return False, None

    in_queue = bool(data.get("isInMergeQueue"))
    entry = data.get("mergeQueueEntry") or {}
    desc = None
    if in_queue and entry:
        state = str(entry.get("state", "")).lower()
        pos = entry.get("position")
        desc = f"#{pos} {state}" if pos is not None else state
    return in_queue, desc


def show_failure_logs(checks: CheckSummary) -> None:
    print()
    print_error("Failed checks:")
    for name in checks.failed_names:
        print_error(f"  ✗ {name}")
    if shutil.which("nbo"):
        print_header("nbo log")
        run(["nbo", "log"], check=False)
        print()


def format_elapsed(start: float) -> str:
    secs = int(time.monotonic() - start)
    return f"{secs // 60:d}:{secs % 60:02d}"


class StatusLine:
    """Single self-updating line on a TTY; one line per change otherwise."""

    def __init__(self) -> None:
        self.last_key: object = None
        self.active = False

    def update(self, key: object, text: str) -> None:
        if IS_TTY:
            sys.stdout.write(f"{Colors.CLEAR_LINE}{text}")
            sys.stdout.flush()
            self.active = True
        elif key != self.last_key:
            print(text, flush=True)
        self.last_key = key

    def finish(self) -> None:
        if self.active:
            print()
            self.active = False


def wait_for_merge_gitea(pr_id: str) -> bool:
    status = StatusLine()
    start = time.monotonic()
    try:
        while True:
            result = check_gitea_pr_state(pr_id)
            if result is not None:
                return result
            status.update("waiting", f"[{format_elapsed(start)}] Waiting for merge...")
            time.sleep(30)
    finally:
        status.finish()


def wait_for_merge_github(repo: Repo, pr_id: str) -> bool:
    status = StatusLine()
    start = time.monotonic()
    logs_shown = False
    automerge_seen = False
    consecutive_errors = 0
    try:
        while True:
            pr_data = get_pr_status_github(pr_id)
            if pr_data is None:
                # `gh` occasionally hits transient API errors; don't give up
                # on the first one after possibly waiting for a long time.
                consecutive_errors += 1
                if consecutive_errors >= 5:
                    status.finish()
                    print_error("Failed to get PR status")
                    return False
                time.sleep(10)
                continue
            consecutive_errors = 0

            checks = summarize_checks(pr_data.get("statusCheckRollup") or [])
            in_queue, queue_desc = get_merge_queue_status_github(
                repo, int(pr_data.get("number", 0))
            )
            automerge_seen |= in_queue or pr_data.get("autoMergeRequest") is not None

            line = f"[{format_elapsed(start)}] Checks: {checks.render()}"
            if in_queue:
                line += f"  {Colors.BLUE}[merge queue {queue_desc}]{Colors.RESET}"
            status.update((checks.render(), queue_desc), line)

            if checks.failed and not checks.pending and not logs_shown:
                status.finish()
                show_failure_logs(checks)
                logs_shown = True

            completion = check_pr_completion(
                pr_data, checks, in_merge_queue=in_queue, automerge_seen=automerge_seen
            )
            if completion is not None:
                status.finish()
                success, message = completion
                if not success:
                    print_error(f"✗ {message}: {pr_data.get('url', '')}")
                return success

            time.sleep(10)
    finally:
        status.finish()
        if IS_TTY:
            sys.stdout.write("\a")
            sys.stdout.flush()


def split_message(msg: str) -> tuple[str, str]:
    # Drop git-style comment lines so the editor template can carry hints.
    lines = [ln for ln in msg.splitlines() if not ln.startswith("#")]
    text = "\n".join(lines).strip()
    title, _, body = text.partition("\n")
    return title.strip(), body.strip()


def commit_log(default_branch: str) -> str:
    return run(
        [
            "git",
            "log",
            "--reverse",
            "--pretty=format:%s%n%n%b%n",
            f"origin/{default_branch}..HEAD",
        ],
        capture=True,
    ).stdout


def edit_message(initial: str) -> str:
    template = (
        f"{initial.rstrip()}\n\n"
        "# First line: PR title. Rest: PR body. Lines starting with '#' are\n"
        "# ignored. An empty title aborts.\n"
    )
    with tempfile.NamedTemporaryFile(
        mode="w", prefix="PR_EDITMSG_", suffix=".gitcommit", delete=False
    ) as f:
        f.write(template)
        path = Path(f.name)
    try:
        editor = os.environ.get("VISUAL") or os.environ.get("EDITOR", "vim")
        subprocess.run([*editor.split(), str(path)], check=True)
        # Re-open by path: editors typically replace the file via rename, so
        # the original file descriptor would still see the old content.
        return path.read_text()
    finally:
        path.unlink(missing_ok=True)


def get_pr_message(message_arg: str | None, default_branch: str) -> tuple[str, str]:
    if message_arg:
        return split_message(message_arg)
    msg = commit_log(default_branch)
    # Without a terminal no editor can run; use the commit messages verbatim.
    if sys.stdin.isatty() and IS_TTY:
        msg = edit_message(msg)
    return split_message(msg)


def worktree_clean() -> bool:
    return run(["git", "diff", "HEAD", "--quiet"], check=False).returncode == 0


def prepare_repository(default_branch: str) -> bool:
    """Pull and format-check. Returns True if there is something to merge."""
    print_header(f"Rebasing onto origin/{default_branch}")
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
            "--quiet",
            "origin",
            default_branch,
        ]
    )
    run(["git", "submodule", "--quiet", "update", "--init", "--recursive"], check=False)

    result = run(["git", "diff", "--quiet", f"origin/{default_branch}"], check=False)
    if result.returncode == 0:
        print_success("✓ Nothing to merge, branch is identical to upstream")
        return False

    print_header("Checking formatting")
    fmt_ok = run(["flake-fmt"], check=False).returncode == 0
    if fmt_ok and worktree_clean():
        print_success("✓ Formatting ok")
        return True
    print_warning("Formatter changed files; folding fixes into commits with git-absorb")
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
    if worktree_clean():
        print_success("✓ Formatting fixes absorbed; re-run to verify and push")
    elif sys.stdin.isatty() and IS_TTY:
        print_warning("Unabsorbed changes remain; opening lazygit")
        run(["lazygit"], check=False)
    else:
        print_error(
            "Formatting changes remain uncommitted. Run 'flake-fmt' and commit."
        )
    return False


def push_branch(default_branch: str) -> str:
    """Push HEAD and return the branch name to use for the PR."""
    current_branch = run(
        ["git", "branch", "--show-current"], capture=True
    ).stdout.strip()

    if not current_branch or current_branch == default_branch:
        branch_name = f"merge-when-green-{os.environ.get('USER', 'user')}"
    else:
        branch_name = current_branch

    print_header(f"Pushing to origin/{branch_name}")
    run(["git", "push", "--force", "--quiet", "origin", f"HEAD:{branch_name}"])
    print_success("✓ Pushed")
    return branch_name


def open_or_reuse_pr(
    repo: Repo,
    platform: Platform,
    branch: str,
    default_branch: str,
    message: str | None,
) -> str:
    print_header("Pull request")
    if platform == Platform.GITHUB:
        existing = github_find_open_pr(branch)
        if existing:
            print_success(f"✓ Reusing {Colors.BLUE}{existing['url']}{Colors.RESET}")
            github_enable_automerge(branch)
            return branch
    else:
        existing = gitea_find_open_pr(branch)
        if existing:
            pr_id = str(existing["index"])
            url = existing.get("html_url", f"#{pr_id}")
            print_success(f"✓ Reusing {Colors.BLUE}{url}{Colors.RESET}")
            gitea_enable_automerge(repo, pr_id)
            return pr_id

    title, body = get_pr_message(message, default_branch)
    if not title:
        print_error("✗ Empty PR title, aborting")
        raise SystemExit(1)
    print_subtle(f"Title: {title}")
    if platform == Platform.GITHUB:
        return create_pr_github(branch, default_branch, title, body)
    return create_pr_gitea(repo, branch, default_branch, title, body)


def finalize_merge(
    repo: Repo, platform: Platform, pr_id: str, default_branch: str
) -> int:
    print_header("Waiting for merge (Ctrl-C to stop watching; auto-merge stays on)")
    if platform == Platform.GITHUB:
        merged = wait_for_merge_github(repo, pr_id)
    else:
        merged = wait_for_merge_gitea(pr_id)
    if not merged:
        return 1
    print_success("✓ PR merged")
    run(["git", "fetch", "--quiet", "origin", default_branch])
    run(["git", "rebase", "--quiet", f"origin/{default_branch}"])
    print_success(f"✓ Rebased onto origin/{default_branch}")
    return 0


def chdir_repo_root() -> None:
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
    repo = get_repo_info()
    platform = detect_platform(repo)
    default_branch = get_default_branch(platform)

    if not prepare_repository(default_branch):
        return 1

    branch_name = push_branch(default_branch)
    pr_id = open_or_reuse_pr(repo, platform, branch_name, default_branch, args.message)

    if args.no_wait:
        return 0
    return finalize_merge(repo, platform, pr_id, default_branch)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except KeyboardInterrupt:
        print_warning("\nInterrupted")
        sys.exit(130)
    except subprocess.CalledProcessError as e:
        # Captured commands would otherwise fail silently
        print_error(f"\n✗ Command failed: {' '.join(map(str, e.cmd))}")
        if e.stderr:
            print_error(e.stderr.strip())
        if e.output:
            print_error(e.output.strip())
        sys.exit(e.returncode)
