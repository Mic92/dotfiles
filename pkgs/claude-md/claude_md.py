#!/usr/bin/env python3
import argparse
import shutil
import subprocess
import sys
from pathlib import Path


def get_repo_root() -> Path:
    """Get the root of the current git repository."""
    try:
        result = subprocess.run(
            ["git", "rev-parse", "--show-toplevel"],
            capture_output=True,
            text=True,
            check=True,
        )
        return Path(result.stdout.strip())
    except subprocess.CalledProcessError:
        print("Error: Not in a git repository", file=sys.stderr)
        sys.exit(1)


def get_claude_md_repo() -> Path:
    """Get the claude.md repository path, creating it if it doesn't exist."""
    claude_md_path = Path.home() / "git" / "claude.md"
    if not claude_md_path.exists():
        print(f"Creating claude.md repository at {claude_md_path}")
        claude_md_path.mkdir(parents=True, exist_ok=True)

        # Initialize git repository
        try:
            subprocess.run(
                ["git", "init"],
                cwd=claude_md_path,
                check=True,
                capture_output=True,
            )
            print(f"✓ Initialized git repository at {claude_md_path}")
        except subprocess.CalledProcessError as e:
            print(f"Error: Failed to initialize git repository: {e}", file=sys.stderr)
            sys.exit(1)
    return claude_md_path


def show_diff(file1: Path, file2: Path) -> None:
    """Show diff between two files."""
    try:
        result = subprocess.run(
            ["diff", "-u", str(file1), str(file2)],
            capture_output=True,
            text=True,
            check=False,
        )
        if result.stdout:
            print(f"Diff between {file1} and {file2}:")
            print(result.stdout)
    except subprocess.CalledProcessError:
        pass


def handle_claude_dir(repo_root: Path, claude_md_repo: Path, repo_name: str) -> None:
    """Handle the .claude directory."""
    local_claude_dir = repo_root / ".claude"
    claude_md_claude_dir = claude_md_repo / repo_name / ".claude"

    # If both exist, check if they're linked
    if local_claude_dir.exists() and claude_md_claude_dir.exists():
        if (
            local_claude_dir.is_symlink()
            and local_claude_dir.resolve() == claude_md_claude_dir.resolve()
        ):
            print(f"✓ {local_claude_dir} is already linked to {claude_md_claude_dir}")
            return

        print(
            f"Error: Conflict - both {local_claude_dir} and {claude_md_claude_dir} exist",
            file=sys.stderr,
        )
        sys.exit(1)

    # If directory exists in claude.md repo but not locally
    if claude_md_claude_dir.exists() and not local_claude_dir.exists():
        local_claude_dir.symlink_to(claude_md_claude_dir)
        print(f"✓ Created symlink: {local_claude_dir} -> {claude_md_claude_dir}")
        return

    # If directory exists locally but not in claude.md repo
    if local_claude_dir.exists() and not claude_md_claude_dir.exists():
        # Create parent directory if needed
        claude_md_claude_dir.parent.mkdir(parents=True, exist_ok=True)

        # Copy entire directory
        shutil.copytree(local_claude_dir, claude_md_claude_dir)
        print(f"✓ Copied {local_claude_dir} to {claude_md_claude_dir}")

        # Remove original and create symlink
        shutil.rmtree(local_claude_dir)
        local_claude_dir.symlink_to(claude_md_claude_dir)
        print(f"✓ Created symlink: {local_claude_dir} -> {claude_md_claude_dir}")

        # Stage the directory in claude.md repo (use -f to force add)
        try:
            subprocess.run(
                [
                    "git",
                    "add",
                    "-f",
                    str(claude_md_claude_dir.relative_to(claude_md_repo)),
                ],
                cwd=claude_md_repo,
                check=True,
            )
            print(f"✓ Staged {claude_md_claude_dir} in claude.md repository")
        except subprocess.CalledProcessError:
            print(
                f"Warning: Could not stage {claude_md_claude_dir} in git",
                file=sys.stderr,
            )


def add_command() -> None:
    """Handle the 'add' command."""
    # Get paths
    repo_root = get_repo_root()
    repo_name = repo_root.name
    claude_md_repo = get_claude_md_repo()

    # File paths
    local_file = repo_root / "CLAUDE.local.md"
    claude_md_dir = claude_md_repo / repo_name
    claude_md_file = claude_md_dir / "CLAUDE.local.md"

    # Handle .claude directory first
    handle_claude_dir(repo_root, claude_md_repo, repo_name)

    # Check if both files exist (conflict)
    if local_file.exists() and claude_md_file.exists():
        # Check if they're already linked
        if local_file.is_symlink() and local_file.resolve() == claude_md_file.resolve():
            print(f"✓ {local_file} is already linked to {claude_md_file}")
            return

        # Show diff and error
        show_diff(local_file, claude_md_file)
        print(
            f"Error: Conflict - both {local_file} and {claude_md_file} exist",
            file=sys.stderr,
        )
        sys.exit(1)

    # If file exists in claude.md repo but not locally
    if claude_md_file.exists() and not local_file.exists():
        # Create symlink
        local_file.symlink_to(claude_md_file)
        print(f"✓ Created symlink: {local_file} -> {claude_md_file}")
        return

    # If file exists locally but not in claude.md repo
    if local_file.exists() and not claude_md_file.exists():
        # Create directory if needed
        claude_md_dir.mkdir(parents=True, exist_ok=True)

        # Copy file to claude.md repo
        shutil.copy2(local_file, claude_md_file)
        print(f"✓ Copied {local_file} to {claude_md_file}")

        # Remove original and create symlink
        local_file.unlink()
        local_file.symlink_to(claude_md_file)
        print(f"✓ Created symlink: {local_file} -> {claude_md_file}")

        # Stage the new file in claude.md repo (use -f to force add)
        try:
            subprocess.run(
                ["git", "add", "-f", str(claude_md_file.relative_to(claude_md_repo))],
                cwd=claude_md_repo,
                check=True,
            )
            print(f"✓ Staged {claude_md_file} in claude.md repository")
        except subprocess.CalledProcessError:
            print(
                f"Warning: Could not stage {claude_md_file} in git",
                file=sys.stderr,
            )
        return

    # Neither file exists - create one with claude CLI
    print("No CLAUDE.local.md file found, creating one with Claude CLI...")

    # Create CLAUDE.local.md with claude CLI in the current repository
    try:
        with local_file.open("w") as f:
            subprocess.run(
                [
                    "claude",
                    "--print",
                    f"Create a CLAUDE.local.md file for the {repo_name} repository with helpful context and instructions",
                ],
                stdout=f,
                cwd=repo_root,
                check=True,
            )
        print(f"✓ Created {local_file} with Claude CLI")

        # Create directory if needed
        claude_md_dir.mkdir(parents=True, exist_ok=True)

        # Move file to claude.md repo
        shutil.move(str(local_file), str(claude_md_file))
        print(f"✓ Moved {local_file} to {claude_md_file}")

        # Create symlink
        local_file.symlink_to(claude_md_file)
        print(f"✓ Created symlink: {local_file} -> {claude_md_file}")

        # Stage the new file in claude.md repo (use -f to force add)
        try:
            subprocess.run(
                ["git", "add", "-f", str(claude_md_file.relative_to(claude_md_repo))],
                cwd=claude_md_repo,
                check=True,
            )
            print(f"✓ Staged {claude_md_file} in claude.md repository")
        except subprocess.CalledProcessError:
            print(
                f"Warning: Could not stage {claude_md_file} in git",
                file=sys.stderr,
            )
    except subprocess.CalledProcessError as e:
        print(f"Error: Failed to create file with Claude CLI: {e}", file=sys.stderr)
        sys.exit(1)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Manage CLAUDE.local.md files across repositories"
    )
    subparsers = parser.add_subparsers(dest="command", help="Available commands")

    # Add command
    subparsers.add_parser(
        "add", help="Add/link CLAUDE.local.md file for current repository"
    )

    args = parser.parse_args()

    if args.command == "add":
        add_command()
    else:
        parser.print_help()
        sys.exit(1)


if __name__ == "__main__":
    main()
