#!/usr/bin/env python3

import os
import sys
import re
import subprocess
from pathlib import Path
from urllib.parse import urlparse
from typing import List, NoReturn


def die(msg: str) -> NoReturn:
    print(msg, file=sys.stderr)
    sys.exit(1)


def sh(cmd: List[str]) -> None:
    print(f"$ {' '.join(cmd)}")
    proc = subprocess.run(cmd)
    if proc.returncode != 0:
        die(f"command failed with {proc.returncode}")


def checkout_repo(repo, path: Path, commit: str) -> None:
    if not os.path.exists(path):
        sh(["git", "clone", "--recurse-submodules", repo, str(path)])

    os.chdir(path)

    sh(["git", "fetch", "origin"])
    sh(["git", "reset", "--hard", commit])
    sh(["git", "submodule", "update", "--init", "--recursive"])
    sh(["git", "clean", "-dfx"])


def main() -> None:
    args = os.environ.get("SSH_ORIGINAL_COMMAND", "").split()
    if len(args) != 3:
        print(args)
        die("USAGE: <url> <commit> <host>")
    url_str, commit, host = args

    if not re.fullmatch(r"[a-z0-9]{40}", commit):
        die(f"invalid hash '{commit}' passed",)

    home = os.environ.get("HOME", None)
    if not home:
        die("$HOME is not set")

    url = urlparse(url_str)

    if url.netloc == "":
        die(f"No hostname given in url: {url_str}")

    if url.scheme not in ["ssh", "git", "https"]:
        die(
            f"Invalid protocol {url.scheme} in {url}:"
            "Only support ssh://, https://, git://"
        )

    sanitized_path = os.path.normpath(url.path).lstrip("/")
    repo_path = Path(home, url.netloc, sanitized_path)
    host_path = Path(repo_path, host)
    if not host_path.relative_to(repo_path):
        die(f"host {host} is not in {repo_path}")

    checkout_repo(url_str, repo_path, commit)

    if not host_path.exists():
        die(f"{host_path} does not exists")
    sh(["nix-build", "--", str(host_path)])
    # TODO sandbox this
    sys.stderr.flush()
    sh(["./result"])


if __name__ == "__main__":
    main()
