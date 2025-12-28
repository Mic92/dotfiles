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
from dataclasses import dataclass
from pathlib import Path


def run(
    cmd: list[str], capture: bool = True, check: bool = True
) -> subprocess.CompletedProcess[str]:
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
        return url.removeprefix("git@github.com:").removesuffix(".git")

    # Handle HTTPS URLs: https://github.com/owner/repo.git
    if "github.com" in url:
        parts = url.split("github.com/")
        if len(parts) > 1:
            return parts[1].removesuffix(".git")

    return None


@dataclass
class GitHubRepoInfo:
    """Information about a GitHub repository."""

    description: str = ""
    is_private: bool = False

    @classmethod
    def fetch(cls, repo: str) -> "GitHubRepoInfo":
        """Fetch repository info from GitHub API."""
        try:
            result = run(
                [
                    "gh",
                    "repo",
                    "view",
                    repo,
                    "--json",
                    "description,isPrivate",
                ]
            )
            data = json.loads(result.stdout)
            return cls(
                description=data.get("description") or "",
                is_private=bool(data.get("isPrivate", False)),
            )
        except (subprocess.CalledProcessError, json.JSONDecodeError):
            return cls()


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
        payload: dict[str, dict[str, str]] = json.loads(result.stdout)
        name = payload.get("xyz.radicle.project", {}).get("name")
        return str(name) if name else None
    except (subprocess.CalledProcessError, json.JSONDecodeError):
        return None


class RadInitError(Exception):
    """Failed to get RID after rad init."""


# Personal seed nodes that should have access to private repos
PRIVATE_SEED_NODES = [
    "did:key:z6MktZckvzz29eJtUQ4u9bkNu8jihg1sRvknUZMm1xq2stn9",  # eve
    "did:key:z6MkwQTGzGVFjmT54Ustr82rc3bMGkjSjeCXQWgSvNNvVnwa",  # eva
    "did:key:z6Mkkmnifhqr7bJ48tKjE3KRXKwH9SSwMavNPfsphCpeT94W",  # blob64
]


def allow_private_seeds() -> None:
    """Allow personal seed nodes to access the private repository."""
    for did in PRIVATE_SEED_NODES:
        run(["rad", "repo", "allow", did], check=False)


def rad_init(
    name: str, description: str, default_branch: str = "main", *, private: bool = False
) -> str:
    """Initialize a new Radicle project."""
    cmd = [
        "rad",
        "init",
        "--name",
        name,
        "--description",
        description,
        "--default-branch",
        default_branch,
        "--private" if private else "--public",
    ]
    run(cmd)
    rid = get_rad_rid()
    if not rid:
        msg = "Failed to get RID after rad init"
        raise RadInitError(msg)
    if private:
        allow_private_seeds()
    return rid


def create_machine_identity(rad_home: Path, alias: str) -> dict[str, str]:
    """Create a new Radicle identity for GitHub Actions."""
    # Create identity with empty passphrase
    subprocess.run(
        ["rad", "auth", "--alias", alias, "--stdin"],
        input="",
        text=True,
        check=True,
    )

    # Copy preferred seeds from main config
    main_rad_home = Path.home() / ".radicle"
    main_config = main_rad_home / "config.json"
    temp_config = rad_home / "config.json"
    if main_config.exists() and temp_config.exists():
        main_cfg = json.loads(main_config.read_text())
        temp_cfg = json.loads(temp_config.read_text())
        if "preferredSeeds" in main_cfg:
            temp_cfg["preferredSeeds"] = main_cfg["preferredSeeds"]
            temp_config.write_text(json.dumps(temp_cfg, indent=2))

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
    run(
        [
            "rad",
            "id",
            "update",
            "--title",
            title,
            "--description",
            "Machine account for GitHub Actions mirroring",
            "--delegate",
            did,
            "--threshold",
            "1",
        ]
    )


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
    set_github_secret(
        repo, "RADICLE_IDENTITY_PASSPHRASE", identity["passphrase_b64"], env=env_name
    )
    set_github_secret(
        repo, "RADICLE_IDENTITY_PRIVATE_KEY", identity["private_key_b64"], env=env_name
    )
    set_github_secret(
        repo, "RADICLE_IDENTITY_PUBLIC_KEY", identity["public_key_b64"], env=env_name
    )

    # Repository secrets
    set_github_secret(repo, "RADICLE_PROJECT_NAME", project_name)
    set_github_secret(repo, "RADICLE_REPOSITORY_ID", rid)


def get_preferred_seeds() -> list[str]:
    """Get preferred seeds from the main radicle config."""
    main_config = Path.home() / ".radicle" / "config.json"
    if main_config.exists():
        try:
            cfg = json.loads(main_config.read_text())
        except json.JSONDecodeError:
            return []
        else:
            seeds: list[str] = cfg.get("preferredSeeds", [])
            return seeds
    return []


def create_workflow_content(preferred_seeds: list[str] | None = None) -> str:
    """Generate the workflow file content."""
    if preferred_seeds is None:
        preferred_seeds = get_preferred_seeds()

    seeds_line = ""
    if preferred_seeds:
        seeds_str = ",".join(preferred_seeds)
        seeds_line = f'\n          preferred-seeds: "{seeds_str}"'

    return f"""\
name: Mirror to Radicle

on:
  push:
    branches:
      - main
  workflow_dispatch:

jobs:
  mirror:
    runs-on: ubuntu-latest
    environment: radicle
    steps:
      - id: mirror
        uses: Mic92/mirror-to-radicle@main
        with:
          radicle-identity-alias: "${{{{{{ secrets.RADICLE_IDENTITY_ALIAS }}}}}}"
          radicle-identity-passphrase: "${{{{{{ secrets.RADICLE_IDENTITY_PASSPHRASE }}}}}}"
          radicle-identity-private-key: "${{{{{{ secrets.RADICLE_IDENTITY_PRIVATE_KEY }}}}}}"
          radicle-identity-public-key: "${{{{{{ secrets.RADICLE_IDENTITY_PUBLIC_KEY }}}}}}"
          radicle-project-name: "${{{{{{ secrets.RADICLE_PROJECT_NAME }}}}}}"
          radicle-repository-id: "${{{{{{ secrets.RADICLE_REPOSITORY_ID }}}}}}"{seeds_line}
"""


def create_workflow_file(workflow_dir: Path | None = None) -> Path:
    """Create the GitHub Actions workflow file."""
    if workflow_dir is None:
        workflow_dir = Path(".github/workflows")

    workflow_dir.mkdir(parents=True, exist_ok=True)
    workflow_file = workflow_dir / "radicle.yaml"
    workflow_file.write_text(create_workflow_content())
    return workflow_file


def parse_args() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Set up automatic GitHub to Radicle mirroring"
    )
    parser.add_argument(
        "--name",
        help="Project name (default: derived from repo name)",
    )
    parser.add_argument(
        "--description",
        help="Project description (default: from GitHub)",
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
    parser.add_argument(
        "--private",
        action="store_true",
        help="Mirror as private (auto-detected from GitHub if not specified)",
    )
    return parser.parse_args()


@dataclass
class MirrorConfig:
    """Configuration for setting up a Radicle mirror."""

    github_repo: str
    project_name: str
    description: str
    alias: str
    skip_secrets: bool = False
    skip_workflow: bool = False
    private: bool = False


def setup_mirror(config: MirrorConfig) -> int:
    """Set up the Radicle mirror for a GitHub repository."""
    print(f"Setting up Radicle mirror for {config.github_repo}")

    project_name = config.project_name

    # Check if rad remote already exists
    if has_rad_remote():
        rid = get_rad_rid()
        project_name = get_rad_project_name() or project_name
        print(f"Radicle project already exists: {rid}")
    else:
        visibility = "private" if config.private else "public"
        print(f"Initializing new Radicle project: {project_name} ({visibility})")
        rid = rad_init(project_name, config.description, private=config.private)
        print(f"Created Radicle project: {rid}")

    if not rid:
        print("Error: Could not determine RID", file=sys.stderr)
        return 1

    # Create machine identity in a temporary directory
    with tempfile.TemporaryDirectory(prefix="rad-actions-") as tmpdir:
        rad_home = Path(tmpdir)
        os.environ["RAD_HOME"] = str(rad_home)

        print(f"Creating machine identity: {config.alias}")
        identity = create_machine_identity(rad_home, config.alias)
        print(f"Machine DID: {identity['did']}")

        # Add as delegate (using main identity)
        del os.environ["RAD_HOME"]
        print("Adding machine identity as delegate...")
        add_delegate(identity["did"])

        # Sync
        print("Syncing to network...")
        sync_to_seeds()

        # Set up GitHub secrets
        if not config.skip_secrets:
            print("Setting up GitHub secrets...")
            setup_github_secrets(config.github_repo, identity, project_name, rid)

    # Create workflow file
    if not config.skip_workflow:
        workflow_file = create_workflow_file()
        print(f"Created workflow: {workflow_file}")

    print("\nSetup complete!")
    print(f"  RID: {rid}")
    print(f"  GitHub repo: {config.github_repo}")
    if not config.skip_workflow:
        print("  Workflow: .github/workflows/radicle.yaml")
    print("\nNext steps:")
    print("  1. Commit and push the workflow file")
    print("  2. The mirror will sync on every push")

    return 0


def main() -> int:
    args = parse_args()

    github_repo = get_github_repo()
    if not github_repo:
        print("Error: Not in a GitHub repository", file=sys.stderr)
        return 1

    # Fetch repo info once from GitHub API
    repo_info = GitHubRepoInfo.fetch(github_repo)

    repo_name = github_repo.split("/")[-1]
    config = MirrorConfig(
        github_repo=github_repo,
        project_name=args.name or repo_name,
        description=args.description or repo_info.description,
        alias=args.alias or f"{repo_name}_actions",
        skip_secrets=args.skip_secrets,
        skip_workflow=args.skip_workflow,
        private=args.private or repo_info.is_private,
    )

    return setup_mirror(config)


if __name__ == "__main__":
    sys.exit(main())
