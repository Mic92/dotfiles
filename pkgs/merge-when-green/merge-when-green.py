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
    # Remove merge-when-green header since it doesn't need emoji
    if message == "merge-when-green":
        print(f"\n{Colors.BOLD}{message}{Colors.RESET}")
    else:
        print(f"\n{Colors.BOLD}📌 {message}{Colors.RESET}")


def print_subtle(message: str) -> None:
    """Print a subtle message in gray."""
    print(f"{Colors.GRAY}{message}{Colors.RESET}")


def print_command(cmd_str: str) -> None:
    """Print a command in blue."""
    print(f"{Colors.BLUE}🚀 {cmd_str}{Colors.RESET}", file=sys.stderr)


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
    capture_output: bool = True,
    silent: bool = False,
) -> subprocess.CompletedProcess:
    """Run a command and return the result."""
    if not silent:
        log_command(cmd)

    if capture_output:
        result = subprocess.run(cmd, check=False, capture_output=True, text=True)
        if result.returncode != 0 and check:
            print_error(f"Command failed with exit code {result.returncode}")
            if result.stderr:
                print_error(f"stderr: {result.stderr}")
            raise subprocess.CalledProcessError(
                result.returncode, cmd, result.stdout, result.stderr
            )
        return result
    return subprocess.run(cmd, check=check, text=True)


def get_git_remote() -> str:
    """Get the appropriate git remote (upstream or origin)."""
    result = run_command(["git", "remote"], silent=True)
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
        ]
    )
    return result.stdout.strip()


def has_changes(remote: str, target_branch: str) -> bool:
    """Check if there are any changes to merge."""
    result = run_command(
        ["git", "diff", "--quiet", f"{remote}/{target_branch}"], check=False
    )
    return result.returncode != 0


def run_treefmt(target_branch: str) -> bool:
    """Run treefmt if available. Returns True if successful or not needed."""
    print_header("Checking code formatting...")

    # Try to run treefmt directly
    if shutil.which("treefmt"):
        result = run_command(["treefmt", "--fail-on-change"], check=False)
        if result.returncode == 0:
            print_success("✅ Code formatting check passed")
            return True
        print_warning("🔧 Code formatting issues detected")

    # Check if treefmt is in the flake
    current_system = run_command(
        ["nix", "config", "show", "system"], silent=True
    ).stdout.strip()

    has_treefmt_check = f'(val: val ? {current_system} && (val.{current_system}.name == "treefmt" || val.{current_system}.name == "treefmt-nix"))'
    check_result = run_command(
        ["nix", "eval", ".#formatter", "--apply", has_treefmt_check], check=False
    )

    if check_result.stdout.strip() != "true":
        print_subtle("📋 No treefmt configuration found")
        return True  # No treefmt, that's fine

    # Build and run treefmt
    print_warning("🔨 Building treefmt from flake...")
    build_result = run_command(
        [
            "nix",
            "build",
            "-o",
            ".git/treefmt",
            f".#formatter.{current_system}",
            "--print-out-paths",
        ]
    )
    formatter_path = build_result.stdout.strip()

    result = run_command(
        [f"{formatter_path}/bin/treefmt", "--fail-on-change"], check=False
    )
    if result.returncode == 0:
        print_success("✅ Code formatting check passed")
        return True

    # If formatting failed, try to absorb changes
    print_warning("🔧 Attempting to fix formatting issues...")
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
    run_command(["lazygit"], check=False, capture_output=False)
    return False


def create_pr(branch: str, target_branch: str) -> None:
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
        ]
    )

    # Open editor for commit message
    with tempfile.NamedTemporaryFile(
        mode="w+", suffix="_COMMIT_EDITMSG", delete=False
    ) as f:
        f.write(result.stdout)
        f.flush()

        editor = os.environ.get("EDITOR", "vim")
        print_warning(f"✏️  Opening {editor} to edit PR description...")
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
    print_warning("🔄 Enabling auto-merge...")
    run_command(["gh", "pr", "merge", branch, "--auto", "--rebase"])
    print_success("✅ Pull request created and auto-merge enabled 🚀")


def get_pr_state(branch: str) -> str | None:
    """Get the state of a PR, returns None if PR doesn't exist."""
    result = run_command(
        ["gh", "pr", "view", "--json", "state", "--template", "{{.state}}", branch],
        check=False,
        silent=True,
    )
    if result.returncode == 0:
        return result.stdout.strip()
    return None


def wait_for_checks(branch: str, interval: int = 30) -> tuple[bool, str]:
    """Wait for CI checks to complete. Returns (success, status_message)."""
    print_header(f"Waiting for CI checks to complete on '{branch}'...")

    while True:
        # Get check status - gh pr checks uses different field names
        result = run_command(
            ["gh", "pr", "checks", branch, "--json", "state,name,bucket"],
            check=False,
            silent=True,
        )

        if result.returncode != 0:
            return False, f"Failed to get check status: {result.stderr}"

        try:
            checks = json.loads(result.stdout)
        except json.JSONDecodeError:
            return False, "Failed to parse check status"

        if not checks:
            return False, "No checks found"

        # Check status
        pending = []
        failed = []
        passed = []

        for check in checks:
            name = check.get("name", "unknown")
            state = check.get("state", "unknown")

            # For gh pr checks, state can be: pass, fail, pending, etc.
            if state == "pass":
                passed.append(name)
            elif state == "fail":
                failed.append(name)
            elif state in ["pending", "queued"]:
                pending.append(name)
            else:
                # Unknown states are treated as pending
                pending.append(f"{name} ({state})")

        # Print status
        print(f"\n[{time.strftime('%H:%M:%S')}] Check status:")
        print_info(f"  {Colors.GREEN}Passed: {len(passed)}{Colors.RESET}")
        print_info(f"  {Colors.RED}Failed: {len(failed)}{Colors.RESET}")
        print_info(f"  {Colors.YELLOW}Pending: {len(pending)}{Colors.RESET}")

        if failed:
            print_error("\n❌ Failed checks:")
            for name in failed:
                print_error(f"  ❌ {name}")

        if pending:
            print_warning("\n⏳ Pending checks:")
            for name in pending:
                print_warning(f"  ⏳ {name}")

        # Check if done - all checks should have a definitive state
        total_checks = len(passed) + len(failed) + len(pending)
        if not pending or total_checks == 0:
            if failed:
                return False, f"{len(failed)} checks failed"
            if passed:
                return True, f"All {len(passed)} checks passed"
            return False, "No checks found or all checks in unknown state"

        # Wait before next check
        print_subtle(f"\n⏱️  Waiting {interval} seconds before next check...")
        time.sleep(interval)


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(description="Create PR and merge when CI passes")
    parser.add_argument(
        "--wait", "-w", action="store_true", help="Wait for CI checks to complete"
    )
    parser.add_argument(
        "--interval",
        "-i",
        type=int,
        default=30,
        help="Check interval in seconds (default: 30)",
    )
    args = parser.parse_args()

    print_header("merge-when-green")

    # Check if merge-after-ci exists (clan-project)
    if shutil.which("merge-after-ci"):
        print_warning("🔀 Detected clan-project, delegating to merge-after-ci")
        cmd = ["merge-after-ci", "--no-review"]
        if args.wait:
            # merge-after-ci might have its own wait functionality
            pass
        return subprocess.run(cmd + sys.argv[1:], check=False).returncode

    # Get target branch
    print_header("Getting repository information...")
    target_branch = get_default_branch()
    print_info(f"Target branch: {Colors.BLUE}{target_branch}{Colors.RESET}")

    # Run treefmt
    if not run_treefmt(target_branch):
        # treefmt made changes and opened lazygit
        return 1

    # Pull latest changes
    print_header("Pulling latest changes...")
    run_command(["git", "pull", "--rebase", "origin", target_branch])

    # Check if we have changes
    if not has_changes("origin", target_branch):
        print_success("\n✅ No changes to merge 💤")
        return 0

    # Determine branch name
    username = os.environ.get("USER", "unknown")
    branch = f"merge-when-green-{username}"

    # Check if PR already exists
    pr_state = get_pr_state(branch)
    if pr_state == "OPEN":
        print_warning("\n🔍 Existing PR found, checking status...")
        run_command(["gh", "pr", "checks", target_branch], check=False)

    # Push changes
    print_header("Pushing changes...")
    run_command(["git", "push", "--force", "origin", f"HEAD:{branch}"])

    # Create PR if needed
    if pr_state != "OPEN":
        create_pr(branch, target_branch)
    else:
        print_success("\n✅ Using existing PR 📋")

    # Wait for checks if requested
    if args.wait:
        success, message = wait_for_checks(branch, args.interval)
        if success:
            print_success(f"\n✅ {message} 🎉")
        else:
            print_error(f"\n❌ {message}")
        return 0 if success else 1

    print_success("\n✅ Done! 🎉")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except KeyboardInterrupt:
        print_warning("\n⚠️  Interrupted by user")
        sys.exit(130)
    except subprocess.CalledProcessError as e:
        sys.exit(e.returncode)
