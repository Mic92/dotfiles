#!/usr/bin/env python3
"""
Update third-party packages in ./pkgs directory.

This script auto-discovers updatable packages:
1. Packages with `nix-update-args` file -> run nix-update with those args
2. Packages with `update.py` file -> import and call main()

Usage:
    python3 -m updater [--dry-run] [--package NAME] [--list] [--pr]
"""

import argparse
import importlib.util
import json
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path


@dataclass
class Package:
    name: str
    method: str  # "nix-update" or "custom"
    path: Path
    extra_args: list[str] | None = None


@dataclass
class UpdateResult:
    package: Package
    success: bool
    changed: bool
    old_version: str | None = None
    new_version: str | None = None


def get_flake_root() -> Path:
    """Discover the flake root via git rev-parse.

    When installed via Nix, __file__ is in /nix/store, so we cannot
    derive the repo layout from the script location. Instead we find
    the git toplevel, which is the flake root.
    """
    result = run_cmd(["git", "rev-parse", "--show-toplevel"], check=False)
    if result.returncode != 0:
        sys.exit("Error: not inside a git repository")
    return Path(result.stdout.strip())


def run_cmd(
    cmd: list[str], cwd: Path | None = None, check: bool = True
) -> subprocess.CompletedProcess[str]:
    """Run a command and return the result."""
    return subprocess.run(cmd, cwd=cwd, capture_output=True, text=True, check=check)


def git_has_changes(flake_root: Path) -> bool:
    """Check if there are uncommitted changes."""
    result = run_cmd(["git", "status", "--porcelain"], cwd=flake_root, check=False)
    return bool(result.stdout.strip())


def git_get_changes(flake_root: Path) -> str:
    """Get list of changed files."""
    result = run_cmd(["git", "status", "--porcelain"], cwd=flake_root, check=False)
    return result.stdout.strip()


def get_current_version(pkg: Package) -> str | None:
    """Get current version from srcs.json if it exists."""
    srcs_file = pkg.path / "srcs.json"
    if srcs_file.exists():
        try:
            data: dict[str, str] = json.loads(srcs_file.read_text())
            return data.get("version")
        except json.JSONDecodeError:
            pass
    return None


def discover_packages(pkgs_dir: Path) -> list[Package]:
    """Discover all updatable packages."""
    packages: list[Package] = []

    for pkg_dir in pkgs_dir.iterdir():
        if not pkg_dir.is_dir():
            continue

        # Check for nix-update-args file
        nix_update_args_file = pkg_dir / "nix-update-args"
        if nix_update_args_file.exists():
            args = nix_update_args_file.read_text().strip().split()
            packages.append(
                Package(
                    name=pkg_dir.name,
                    method="nix-update",
                    path=pkg_dir,
                    extra_args=args,
                )
            )
            continue

        # Check for update.py file
        update_script = pkg_dir / "update.py"
        if update_script.exists():
            packages.append(
                Package(
                    name=pkg_dir.name,
                    method="custom",
                    path=pkg_dir,
                )
            )

    return sorted(packages, key=lambda p: p.name)


def run_nix_update(pkg: Package, flake_root: Path, dry_run: bool = False) -> bool:
    """Run nix-update for a package."""
    cmd = [
        "nix-update",
        "--flake",
        f"packages.x86_64-linux.{pkg.name}",
    ]

    if pkg.extra_args:
        cmd.extend(pkg.extra_args)

    print(f"  Running: {' '.join(cmd)}")

    if dry_run:
        print("  (dry-run, skipping)")
        return True

    result = subprocess.run(
        cmd, check=False, cwd=flake_root, capture_output=True, text=True
    )

    if result.returncode != 0:
        print(f"  Error: {result.stderr}")
        return False

    if result.stdout:
        print(f"  {result.stdout.strip()}")

    return True


def run_custom_update(pkg: Package, dry_run: bool = False) -> bool:
    """Import and run main() from update.py."""
    update_script = pkg.path / "update.py"

    print(f"  Running: {update_script}")

    if dry_run:
        print("  (dry-run, skipping)")
        return True

    spec = importlib.util.spec_from_file_location(f"update_{pkg.name}", update_script)
    if spec is None or spec.loader is None:
        print(f"  Error: Could not load {update_script}")
        return False

    module = importlib.util.module_from_spec(spec)
    sys.modules[f"update_{pkg.name}"] = module
    spec.loader.exec_module(module)

    if not hasattr(module, "main"):
        print(f"  Error: {update_script} has no main() function")
        return False

    module.main()
    return True


def update_package(
    pkg: Package, flake_root: Path, dry_run: bool = False
) -> UpdateResult:
    """Update a single package and return the result."""
    print(f"\nUpdating {pkg.name} (method: {pkg.method})...")

    old_version = get_current_version(pkg)

    # Check for existing changes before update
    had_changes_before = git_has_changes(flake_root)

    if pkg.method == "nix-update":
        success = run_nix_update(pkg, flake_root, dry_run)
    elif pkg.method == "custom":
        success = run_custom_update(pkg, dry_run)
    else:
        print(f"  Error: Unknown method: {pkg.method}")
        success = False

    new_version = get_current_version(pkg)
    changed = not had_changes_before and git_has_changes(flake_root)

    return UpdateResult(
        package=pkg,
        success=success,
        changed=changed,
        old_version=old_version,
        new_version=new_version,
    )


def create_pr_for_package(
    pkg: Package, flake_root: Path, dry_run: bool = False
) -> bool:
    """Create a PR for a package update using a git worktree."""
    branch_name = f"update/{pkg.name}"

    print(f"\nCreating PR for {pkg.name}...")

    # Check if branch already exists on remote
    check_branch = run_cmd(
        ["git", "ls-remote", "--heads", "origin", branch_name],
        cwd=flake_root,
        check=False,
    )
    if check_branch.stdout.strip():
        print(f"  Branch {branch_name} already exists on remote, skipping")
        return False

    if dry_run:
        print("  (dry-run, skipping)")
        return True

    return _create_pr_in_worktree(pkg, flake_root, branch_name)


def _create_pr_in_worktree(pkg: Package, flake_root: Path, branch_name: str) -> bool:
    """Create PR using a temporary worktree."""
    with tempfile.TemporaryDirectory() as tmpdir:
        worktree_path = Path(tmpdir) / "worktree"

        # Create worktree with new branch
        result = run_cmd(
            ["git", "worktree", "add", "-b", branch_name, str(worktree_path)],
            cwd=flake_root,
            check=False,
        )
        if result.returncode != 0:
            print(f"  Error creating worktree: {result.stderr}")
            return False

        try:
            return _run_update_and_create_pr(
                pkg, flake_root, worktree_path, branch_name
            )
        finally:
            # Clean up worktree and branch
            run_cmd(
                ["git", "worktree", "remove", "--force", str(worktree_path)],
                cwd=flake_root,
                check=False,
            )
            run_cmd(["git", "branch", "-D", branch_name], cwd=flake_root, check=False)


def _run_update_and_create_pr(
    pkg: Package, flake_root: Path, worktree_path: Path, branch_name: str
) -> bool:
    """Run update in worktree and create PR."""
    old_version = get_current_version(pkg)

    # Run the update in the worktree
    worktree_pkg = Package(
        name=pkg.name,
        method=pkg.method,
        path=worktree_path / "pkgs" / pkg.name,
        extra_args=pkg.extra_args,
    )

    if pkg.method == "nix-update":
        success = run_nix_update(worktree_pkg, worktree_path, dry_run=False)
    elif pkg.method == "custom":
        success = run_custom_update(worktree_pkg, dry_run=False)
    else:
        success = False

    if not success:
        print("  Update failed")
        return False

    if not git_has_changes(worktree_path):
        print("  No changes, already up to date")
        return True  # Not a failure, just nothing to do

    new_version = get_current_version(worktree_pkg)
    old_ver = old_version or "unknown"
    new_ver = new_version or "unknown"

    # Commit and push
    run_cmd(["git", "add", "-A"], cwd=worktree_path)
    commit_msg = f"{pkg.name}: {old_ver} -> {new_ver}"
    commit_result = run_cmd(
        [
            "git",
            "-c",
            "user.name=github-actions[bot]",
            "-c",
            "user.email=41898282+github-actions[bot]@users.noreply.github.com",
            "commit",
            "-m",
            commit_msg,
        ],
        cwd=worktree_path,
        check=False,
    )
    if commit_result.returncode != 0:
        print(f"  Error committing: {commit_result.stderr}")
        return False

    push_result = run_cmd(
        ["git", "push", "-u", "origin", branch_name], cwd=worktree_path, check=False
    )
    if push_result.returncode != 0:
        print(f"  Error pushing: {push_result.stderr}")
        return False

    # Create PR
    pr_body = f"Automated update of {pkg.name} from {old_ver} to {new_ver}."
    pr_result = run_cmd(
        ["gh", "pr", "create", "--title", commit_msg, "--body", pr_body],
        cwd=worktree_path,
        check=False,
    )

    if pr_result.returncode != 0:
        print(f"  Error creating PR: {pr_result.stderr}")
        return False

    print(f"  Created PR: {pr_result.stdout.strip()}")
    return True


def list_packages(packages: list[Package]) -> None:
    """List all discovered packages."""
    print("Packages with nix-update:")
    for pkg in packages:
        if pkg.method == "nix-update":
            args = " ".join(pkg.extra_args or [])
            print(f"  - {pkg.name}" + (f" ({args})" if args else ""))

    print("\nPackages with custom update.py:")
    for pkg in packages:
        if pkg.method == "custom":
            print(f"  - {pkg.name}")


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Update third-party packages in ./pkgs"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be done without making changes",
    )
    parser.add_argument(
        "--package",
        "-p",
        help="Update only the specified package",
    )
    parser.add_argument(
        "--list",
        "-l",
        action="store_true",
        help="List all discovered packages",
    )
    parser.add_argument(
        "--pr",
        action="store_true",
        help="Create a PR for each updated package (uses git worktrees)",
    )

    args = parser.parse_args()

    flake_root = get_flake_root()
    pkgs_dir = flake_root / "pkgs"

    packages = discover_packages(pkgs_dir)

    if args.list:
        list_packages(packages)
        return 0

    if args.package:
        packages = [p for p in packages if p.name == args.package]
        if not packages:
            print(f"Error: Package '{args.package}' not found")
            return 1

    success_count = 0
    failure_count = 0

    for pkg in packages:
        if args.pr:
            # PR mode: use worktree to create PR without touching current checkout
            if create_pr_for_package(pkg, flake_root, args.dry_run):
                success_count += 1
            else:
                failure_count += 1
        else:
            # Normal mode: update in place
            result = update_package(pkg, flake_root, args.dry_run)
            if result.success:
                success_count += 1
            else:
                failure_count += 1

    print(f"\n{'=' * 40}")
    print(f"Results: {success_count} succeeded, {failure_count} failed")

    return 0 if failure_count == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
