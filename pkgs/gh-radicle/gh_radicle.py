#!/usr/bin/env python3
"""
gh-radicle: Set up automatic GitHub to Radicle mirroring.

Creates per-repo Radicle identities for GitHub Actions mirroring,
manages GitHub secrets, and creates the workflow file.
"""

import argparse
import base64
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path


def run(cmd: list[str], capture: bool = True, check: bool = True) -> subprocess.CompletedProcess[str]:
    """Run a command and return the result."""
    return subprocess.run(cmd, capture_output=capture, text=True, check=check)


def get_git_remote_url(remote: str = "origin") -> str | None:
    """Get the URL of a git remote."""
    try:
        result = run(["git", "remote", "get-url", remote])
        return result.stdout.strip()
    except subprocess.CalledProcessError:
        return None


def get_github_repo() -> str | None:
    """Extract GitHub owner/repo from origin remote."""
    url = get_git_remote_url("origin")
    if not url:
        return None

    # Handle SSH URLs: git@github.com:owner/repo.git
    if url.startswith("git@github.com:"):
        repo = url.removeprefix("git@github.com:").removesuffix(".git")
        return repo

    # Handle HTTPS URLs: https://github.com/owner/repo.git
    if "github.com" in url:
        parts = url.split("github.com/")
        if len(parts) > 1:
            return parts[1].removesuffix(".git")

    return None


def has_rad_remote() -> bool:
    """Check if a rad remote exists."""
    return get_git_remote_url("rad") is not None


def get_rad_rid() -> str | None:
    """Get the Radicle Repository ID."""
    try:
        result = run(["rad", "inspect"])
        return result.stdout.strip()
    except subprocess.CalledProcessError:
        return None


def get_rad_project_name() -> str | None:
    """Get the Radicle project name."""
    try:
        result = run(["rad", "inspect", "--payload"])
        payload = json.loads(result.stdout)
        return payload.get("xyz.radicle.project", {}).get("name")
    except (subprocess.CalledProcessError, json.JSONDecodeError):
        return None


def rad_init(name: str, description: str, default_branch: str = "main") -> str:
    """Initialize a new Radicle project."""
    result = run([
        "rad", "init",
        "--name", name,
        "--description", description,
        "--default-branch", default_branch,
        "--public",
    ])
    # Extract RID from output
    rid = get_rad_rid()
    if not rid:
        raise RuntimeError("Failed to get RID after rad init")
    return rid


def create_machine_identity(rad_home: Path, alias: str) -> dict[str, str]:
    """Create a new Radicle identity for GitHub Actions."""
    env = os.environ.copy()
    env["RAD_HOME"] = str(rad_home)

    # Create identity with empty passphrase
    run(["rad", "auth", "--alias", alias, "--stdin"], check=True)

    # Get DID
    result = run(["rad", "self", "--did"])
    did = result.stdout.strip()

    # Read keys
    private_key = (rad_home / "keys" / "radicle").read_text()
    public_key = (rad_home / "keys" / "radicle.pub").read_text()

    return {
        "did": did,
        "alias": alias,
        "private_key_b64": base64.b64encode(private_key.encode()).decode(),
        "public_key_b64": base64.b64encode(public_key.encode()).decode(),
        "passphrase_b64": base64.b64encode(b"").decode(),
    }


def add_delegate(did: str, title: str = "Add GitHub Actions mirror account") -> None:
    """Add a DID as a delegate to the current project."""
    run([
        "rad", "id", "update",
        "--title", title,
        "--description", "Machine account for GitHub Actions mirroring",
        "--delegate", did,
        "--threshold", "1",
    ])


def sync_to_seeds() -> None:
    """Sync the project to seeds."""
    run(["rad", "sync", "--announce"], check=False)


def create_github_environment(repo: str, env_name: str = "radicle") -> None:
    """Create a GitHub environment."""
    run(["gh", "api", f"repos/{repo}/environments/{env_name}", "-X", "PUT"])


def set_github_secret(repo: str, name: str, value: str, env: str | None = None) -> None:
    """Set a GitHub secret."""
    cmd = ["gh", "secret", "set", name, "--repo", repo]
    if env:
        cmd.extend(["--env", env])
    subprocess.run(cmd, input=value, text=True, check=True)


def setup_github_secrets(
    repo: str,
    identity: dict[str, str],
    project_name: str,
    rid: str,
) -> None:
    """Set up all GitHub secrets for Radicle mirroring."""
    env_name = "radicle"

    # Create environment
    create_github_environment(repo, env_name)

    # Environment secrets
    set_github_secret(repo, "RADICLE_IDENTITY_ALIAS", identity["alias"], env=env_name)
    set_github_secret(repo, "RADICLE_IDENTITY_PASSPHRASE", identity["passphrase_b64"], env=env_name)
    set_github_secret(repo, "RADICLE_IDENTITY_PRIVATE_KEY", identity["private_key_b64"], env=env_name)
    set_github_secret(repo, "RADICLE_IDENTITY_PUBLIC_KEY", identity["public_key_b64"], env=env_name)

    # Repository secrets
    set_github_secret(repo, "RADICLE_PROJECT_NAME", project_name)
    set_github_secret(repo, "RADICLE_REPOSITORY_ID", rid)


# Latest version as of 2025-12-28
MIRROR_TO_RADICLE_VERSION = "v0.1.0"

WORKFLOW_TEMPLATE = f"""\
name: Mirror to Radicle

on:
  push:
  workflow_dispatch:

jobs:
  mirror:
    runs-on: ubuntu-slim
    environment: radicle
    steps:
      - id: mirror
        uses: gsaslis/mirror-to-radicle@{MIRROR_TO_RADICLE_VERSION}
        with:
          radicle-identity-alias: "${{ secrets.RADICLE_IDENTITY_ALIAS }}"
          radicle-identity-passphrase: "${{ secrets.RADICLE_IDENTITY_PASSPHRASE }}"
          radicle-identity-private-key: "${{ secrets.RADICLE_IDENTITY_PRIVATE_KEY }}"
          radicle-identity-public-key: "${{ secrets.RADICLE_IDENTITY_PUBLIC_KEY }}"
          radicle-project-name: "${{ secrets.RADICLE_PROJECT_NAME }}"
          radicle-repository-id: "${{ secrets.RADICLE_REPOSITORY_ID }}"
"""


def create_workflow_file(workflow_dir: Path | None = None) -> Path:
    """Create the GitHub Actions workflow file."""
    if workflow_dir is None:
        workflow_dir = Path(".github/workflows")

    workflow_dir.mkdir(parents=True, exist_ok=True)
    workflow_file = workflow_dir / "radicle.yaml"
    workflow_file.write_text(WORKFLOW_TEMPLATE)
    return workflow_file


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Set up automatic GitHub to Radicle mirroring"
    )
    parser.add_argument(
        "--name",
        help="Project name (default: derived from repo name)",
    )
    parser.add_argument(
        "--description",
        default="",
        help="Project description",
    )
    parser.add_argument(
        "--alias",
        help="Alias for the machine identity (default: <repo>_actions)",
    )
    parser.add_argument(
        "--skip-workflow",
        action="store_true",
        help="Skip creating the workflow file",
    )
    parser.add_argument(
        "--skip-secrets",
        action="store_true",
        help="Skip setting up GitHub secrets",
    )
    args = parser.parse_args()

    # Check we're in a git repo
    github_repo = get_github_repo()
    if not github_repo:
        print("Error: Not in a GitHub repository", file=sys.stderr)
        return 1

    repo_name = github_repo.split("/")[-1]
    project_name = args.name or repo_name
    alias = args.alias or f"{repo_name}_actions"

    print(f"Setting up Radicle mirror for {github_repo}")

    # Check if rad remote already exists
    if has_rad_remote():
        rid = get_rad_rid()
        project_name = get_rad_project_name() or project_name
        print(f"Radicle project already exists: {rid}")
    else:
        print(f"Initializing new Radicle project: {project_name}")
        rid = rad_init(project_name, args.description)
        print(f"Created Radicle project: {rid}")

    if not rid:
        print("Error: Could not determine RID", file=sys.stderr)
        return 1

    # Create machine identity in a temporary directory
    with tempfile.TemporaryDirectory(prefix="rad-actions-") as tmpdir:
        rad_home = Path(tmpdir)
        os.environ["RAD_HOME"] = str(rad_home)

        print(f"Creating machine identity: {alias}")
        identity = create_machine_identity(rad_home, alias)
        print(f"Machine DID: {identity['did']}")

        # Add as delegate (using main identity)
        del os.environ["RAD_HOME"]
        print("Adding machine identity as delegate...")
        add_delegate(identity["did"])

        # Sync
        print("Syncing to network...")
        sync_to_seeds()

        # Set up GitHub secrets
        if not args.skip_secrets:
            print("Setting up GitHub secrets...")
            setup_github_secrets(github_repo, identity, project_name, rid)

    # Create workflow file
    if not args.skip_workflow:
        workflow_file = create_workflow_file()
        print(f"Created workflow: {workflow_file}")

    print("\nSetup complete!")
    print(f"  RID: {rid}")
    print(f"  GitHub repo: {github_repo}")
    if not args.skip_workflow:
        print(f"  Workflow: .github/workflows/radicle.yaml")
    print("\nNext steps:")
    print("  1. Commit and push the workflow file")
    print("  2. The mirror will sync on every push")

    return 0


if __name__ == "__main__":
    sys.exit(main())
