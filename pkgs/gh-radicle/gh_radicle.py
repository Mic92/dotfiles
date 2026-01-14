#!/usr/bin/env python3
"""
gh-radicle: Set up GitHub to Radicle mirroring via SSH.

Reads the SSH key from clan vars and sets up GitHub (deploy key, secret, workflow).
The key must already be generated via `clan vars generate`.
"""

import argparse
import json
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path

# Default paths for clan vars - can be overridden via CLI
DEFAULT_DOTFILES = Path.home() / ".homesick/repos/dotfiles"


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


def get_github_url(repo: str) -> str:
    """Get the HTTPS URL for a GitHub repo."""
    return f"https://github.com/{repo}"


@dataclass
class GitHubRepoInfo:
    """Information about a GitHub repository."""

    description: str = ""
    is_private: bool = False
    default_branch: str = "main"

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
                    "description,isPrivate,defaultBranchRef",
                ]
            )
            data = json.loads(result.stdout)
            default_branch = "main"
            if branch_ref := data.get("defaultBranchRef"):
                default_branch = branch_ref.get("name", "main")
            return cls(
                description=data.get("description") or "",
                is_private=bool(data.get("isPrivate", False)),
                default_branch=default_branch,
            )
        except (subprocess.CalledProcessError, json.JSONDecodeError):
            return cls()


def has_rad_remote() -> bool:
    """Check if a rad remote exists."""
    return get_git_remote_url("rad") is not None


def get_rad_rid() -> str | None:
    """Get the Radicle Repository ID (without rad: prefix)."""
    try:
        result = run(["rad", "inspect"])
        rid = result.stdout.strip()
        # Remove rad: prefix if present
        return rid.removeprefix("rad:")
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


def sync_to_seeds() -> None:
    """Sync the project to seeds."""
    run(["rad", "sync", "--announce"], check=False)


def sanitize_name(name: str) -> str:
    """Sanitize repo name for use as identifier."""
    return name.replace("/", "-").replace(".", "-")


def get_clan_var_paths(
    dotfiles_path: Path, machine: str, repo_name: str
) -> tuple[Path, Path]:
    """Get paths to clan vars SSH key files."""
    var_name = f"radicle-sync-{sanitize_name(repo_name)}"
    vars_dir = dotfiles_path / "machines" / machine / "vars" / var_name
    return (
        vars_dir / "ssh-private-key",
        vars_dir / "ssh-public-key",
    )


def add_github_deploy_key(repo: str, title: str, public_key: str) -> None:
    """Add a deploy key to a GitHub repository."""
    subprocess.run(
        [
            "gh",
            "repo",
            "deploy-key",
            "add",
            "-",
            "--repo",
            repo,
            "--title",
            title,
        ],
        input=public_key,
        text=True,
        check=True,
    )


def create_workflow_content(sync_host: str, default_branch: str) -> str:
    """Generate the GitHub Actions workflow file content."""
    return f"""\
name: Radicle Sync
on:
  push:
    branches:
      - {default_branch}
  workflow_dispatch:

jobs:
  sync:
    runs-on: ubuntu-latest
    steps:
      - name: Sync to Radicle
        env:
          SSH_PRIVATE_KEY: ${{{{ secrets.RADICLE_SYNC_SSH_KEY }}}}
        run: |
          mkdir -p ~/.ssh
          echo "$SSH_PRIVATE_KEY" > ~/.ssh/id_ed25519
          chmod 600 ~/.ssh/id_ed25519
          ssh-keyscan {sync_host} >> ~/.ssh/known_hosts
          ssh radicle-sync@{sync_host}
"""


def create_workflow_file(
    sync_host: str, default_branch: str, workflow_dir: Path | None = None
) -> Path:
    """Create the GitHub Actions workflow file."""
    if workflow_dir is None:
        workflow_dir = Path(".github/workflows")

    workflow_dir.mkdir(parents=True, exist_ok=True)
    workflow_file = workflow_dir / "radicle.yaml"
    workflow_file.write_text(create_workflow_content(sync_host, default_branch))
    return workflow_file


def set_github_secret(repo: str, name: str, value: str) -> None:
    """Set a GitHub repository secret."""
    subprocess.run(
        ["gh", "secret", "set", name, "--repo", repo],
        input=value,
        text=True,
        check=True,
    )


def parse_args() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Set up GitHub to Radicle mirroring via SSH"
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
        "--skip-workflow",
        action="store_true",
        help="Skip creating the workflow file",
    )
    parser.add_argument(
        "--skip-github-secret",
        action="store_true",
        help="Skip setting up GitHub secret",
    )
    parser.add_argument(
        "--skip-deploy-key",
        action="store_true",
        help="Skip adding deploy key to GitHub",
    )
    parser.add_argument(
        "--private",
        action="store_true",
        help="Create as private Radicle repo (auto-detected from GitHub if not specified)",
    )
    parser.add_argument(
        "--sync-host",
        default="radicle.thalheim.io",
        help="SSH host for radicle-sync (default: radicle.thalheim.io)",
    )
    parser.add_argument(
        "--dotfiles",
        type=Path,
        default=DEFAULT_DOTFILES,
        help=f"Path to dotfiles repo (default: {DEFAULT_DOTFILES})",
    )
    parser.add_argument(
        "--machine",
        default="eve",
        help="Machine name for clan vars (default: eve)",
    )
    return parser.parse_args()


@dataclass
class SetupConfig:
    """Configuration for setting up Radicle sync."""

    github_repo: str
    github_url: str
    project_name: str
    description: str
    default_branch: str
    private: bool
    skip_workflow: bool
    skip_github_secret: bool
    skip_deploy_key: bool
    sync_host: str
    dotfiles_path: Path
    machine: str


def setup_sync(config: SetupConfig) -> int:
    """Set up Radicle sync for a GitHub repository."""
    print(f"Setting up Radicle sync for {config.github_repo}")

    project_name = config.project_name
    repo_name = sanitize_name(config.github_repo)

    # Check if rad remote already exists
    if has_rad_remote():
        repo_id = get_rad_rid()
        project_name = get_rad_project_name() or project_name
        print(f"Radicle project already exists: {repo_id}")
    else:
        visibility = "private" if config.private else "public"
        print(f"Initializing new Radicle project: {project_name} ({visibility})")
        repo_id = rad_init(
            project_name,
            config.description,
            default_branch=config.default_branch,
            private=config.private,
        )
        print(f"Created Radicle project: {repo_id}")

    if not repo_id:
        print("Error: Could not determine Radicle repo ID", file=sys.stderr)
        return 1

    # Get SSH key from clan vars
    private_key_path, public_key_path = get_clan_var_paths(
        config.dotfiles_path, config.machine, config.github_repo
    )

    if not private_key_path.exists() or not public_key_path.exists():
        print(
            f"\nError: SSH key not found at {private_key_path.parent}", file=sys.stderr
        )
        print(
            "\nFirst, add this to your NixOS config and run 'clan vars generate':",
            file=sys.stderr,
        )
        print(
            f"""
(mkRepo {{
  name = "{repo_name}";
  repoId = "{repo_id}";
  githubUrl = "{config.github_url}";
  branch = "{config.default_branch}";
}})
""",
            file=sys.stderr,
        )
        return 1

    private_key = private_key_path.read_text()
    public_key = public_key_path.read_text().strip()

    print(f"Using SSH key from clan vars: {private_key_path.parent}")

    # Add deploy key to GitHub (read-only is fine, we only fetch)
    if not config.skip_deploy_key:
        print("Adding deploy key to GitHub...")
        try:
            add_github_deploy_key(
                config.github_repo, f"Radicle Sync ({config.machine})", public_key
            )
        except subprocess.CalledProcessError:
            print(
                "Warning: Failed to add deploy key (may already exist)", file=sys.stderr
            )

    # Set GitHub secret for the workflow
    if not config.skip_github_secret:
        print("Setting GitHub secret RADICLE_SYNC_SSH_KEY...")
        set_github_secret(config.github_repo, "RADICLE_SYNC_SSH_KEY", private_key)

    # Sync to seeds
    print("Syncing to network...")
    sync_to_seeds()

    # Create workflow file
    if not config.skip_workflow:
        workflow_file = create_workflow_file(config.sync_host, config.default_branch)
        print(f"Created workflow: {workflow_file}")

    print("\n" + "=" * 60)
    print("Setup complete!")
    print("=" * 60)
    print(f"  Radicle repo: {repo_id}")
    print(f"  GitHub repo:  {config.github_repo}")
    print(f"  SSH key:      {private_key_path.parent}")
    if not config.skip_workflow:
        print("  Workflow:     .github/workflows/radicle.yaml")
    print("\nNext steps:")
    print("  1. Commit and push the workflow file")
    print("  2. Deploy your NixOS configuration")

    return 0


def main() -> int:
    args = parse_args()

    github_repo = get_github_repo()
    if not github_repo:
        print("Error: Not in a GitHub repository", file=sys.stderr)
        return 1

    # Fetch repo info from GitHub API
    repo_info = GitHubRepoInfo.fetch(github_repo)

    repo_name = github_repo.split("/")[-1]
    config = SetupConfig(
        github_repo=github_repo,
        github_url=get_github_url(github_repo),
        project_name=args.name or repo_name,
        description=args.description or repo_info.description,
        default_branch=repo_info.default_branch,
        private=args.private or repo_info.is_private,
        skip_workflow=args.skip_workflow,
        skip_github_secret=args.skip_github_secret,
        skip_deploy_key=args.skip_deploy_key,
        sync_host=args.sync_host,
        dotfiles_path=args.dotfiles,
        machine=args.machine,
    )

    return setup_sync(config)


if __name__ == "__main__":
    sys.exit(main())
