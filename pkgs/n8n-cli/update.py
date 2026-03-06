#!/usr/bin/env python3
"""Update n8n-cli to the latest commit on the fork and regenerate bun.nix."""

import base64
import json
import subprocess
import sys
import tempfile
from pathlib import Path

OWNER = "Mic92"
REPO = "n8n-cli"


def run(cmd: list[str], **kwargs: object) -> subprocess.CompletedProcess[str]:
    print(f"+ {' '.join(cmd)}", file=sys.stderr)
    return subprocess.run(cmd, check=True, text=True, **kwargs)


def get_latest_commit() -> dict[str, str]:
    result = run(
        ["gh", "api", f"repos/{OWNER}/{REPO}/commits/main", "--jq", ".sha"],
        capture_output=True,
    )
    rev = result.stdout.strip()

    # Get version from package.json at that commit
    pkg_result = run(
        [
            "gh",
            "api",
            f"repos/{OWNER}/{REPO}/contents/package.json",
            "--jq",
            ".content",
        ],
        capture_output=True,
    )
    pkg_json = json.loads(base64.b64decode(pkg_result.stdout.strip()))
    version = pkg_json["version"]

    return {"rev": rev, "version": version}


def prefetch_source(rev: str) -> str:
    result = run(
        ["nix", "flake", "prefetch", "--json", f"github:{OWNER}/{REPO}/{rev}"],
        capture_output=True,
    )
    data = json.loads(result.stdout)
    return data["hash"]


def generate_bun_nix(source_path: str, output: Path) -> None:
    """Clone source, patch 'latest' dep, install, and run bun2nix."""
    with tempfile.TemporaryDirectory() as tmpdir:
        workdir = Path(tmpdir) / "n8n-cli"
        run(["cp", "-r", source_path, str(workdir)])
        # Patch @types/bun "latest" -> pinned version from lockfile
        pkg_json = workdir / "package.json"
        text = pkg_json.read_text()
        text = text.replace('"@types/bun": "latest"', '"@types/bun": "*"')
        pkg_json.write_text(text)
        # Install to update lockfile with pinned version
        run(["bun", "install"], cwd=workdir)
        # Generate bun.nix
        run(
            [
                "bun2nix",
                "-l",
                str(workdir / "bun.lock"),
                "-o",
                str(output),
            ]
        )


def main() -> None:
    repo_root = Path.cwd()

    current = json.loads((repo_root / "n8n-cli-source.json").read_text())
    latest = get_latest_commit()

    if current.get("rev") == latest["rev"]:
        print(f"Already up to date: {latest['rev'][:12]}")
        sys.exit(0)

    print(f"Updating {current.get('rev', 'unknown')[:12]} -> {latest['rev'][:12]}")

    hash_ = prefetch_source(latest["rev"])

    source_json = {
        "version": latest["version"],
        "rev": latest["rev"],
        "hash": hash_,
    }

    # Write source.json first so we know what we're building
    source_path = repo_root / "n8n-cli-source.json"
    source_path.write_text(json.dumps(source_json, indent=2) + "\n")
    print(f"Wrote {source_path}")

    # Fetch source to temp dir for bun.nix generation
    result = run(
        [
            "nix",
            "flake",
            "prefetch",
            "--json",
            f"github:{OWNER}/{REPO}/{latest['rev']}",
        ],
        capture_output=True,
    )
    store_path = json.loads(result.stdout)["storePath"]

    bun_nix = repo_root / "bun.nix"
    generate_bun_nix(store_path, bun_nix)
    print(f"Wrote {bun_nix}")

    print(f"\nUpdated to {latest['rev'][:12]}. Test with: nix build .#n8n-cli")


if __name__ == "__main__":
    main()
