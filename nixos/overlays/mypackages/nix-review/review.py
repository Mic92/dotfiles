#!/usr/bin/env python3
# this script requires at least python 3.6!
import sys
if sys.version_info < (3, 6):
    sys.stderr.write("this script requires at least python 3.6")
    sys.exit(1)
import tempfile
import subprocess
import os
import shutil
import xml.etree.ElementTree as ET
import multiprocessing
import json
import urllib.request


def sh(command, **kwargs):
    print("$ " + ' '.join(command))
    subprocess.check_call(command, **kwargs)


def die(message):
    print(message, file=sys.stderr)
    sys.exit(1)


def build_in_path(args, attrs, path):
    if not attrs:
        print('Nothing changed')
        return

    result_dir = tempfile.mkdtemp(prefix='nox-review-')
    print('Building in {}: {}'.format(result_dir, ' '.join(attrs)))
    command = [
        'nix-shell',
        '--keep-going',
        '--max-jobs', str(multiprocessing.cpu_count()),
        # only matters for single-user nix and trusted users
        "--option", "build-use-sandbox", "true"
    ] + args
    for a in attrs:
        command.append(f"-p")
        command.append(a)

    try:
        sh(command, cwd=result_dir)
    except subprocess.CalledProcessError:
        msg = f"The invocation of '{' '.join(command)}' failed\n\n"
        msg += "Your NIX_PATH still points to the merged pull requests, so you can make attempts to fix it and rerun the command above"
        print(msg, file=sys.stderr)
        # XXX personal nit to use bash here,
        # since my zsh overrides NIX_PATH.
        sh(["bash"], cwd=result_dir)


def list_packages(path, check_meta=False):
    cmd = [
        'nix-env', '-f', path, '-qaP', '--xml', '--out-path', '--show-trace'
    ]
    if check_meta:
        cmd.append("--meta")
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    context = ET.iterparse(process.stdout, events=("start", ))
    packages = set()
    for event, elem in context:
        if elem.tag == "item":
            attrib = elem.attrib["attrPath"]
        elif elem.tag == "output":
            assert attrib is not None
            path = elem.attrib["path"]
            packages.add((attrib, path))
    return packages


def fetch_ref(ref):
    try:
        sh(["git", "branch", "-D", f"_nix-review_"])
    except Exception:
        pass
    sh([
        "git", "fetch", "https://github.com/NixOS/nixpkgs",
        f"{ref}:_nix-review_"
    ])
    o = subprocess.check_output(
        ["git", "rev-parse", "--verify", "_nix-review_"])
    return o.strip().decode("utf-8")


def differences(old, new):
    raw = new - old
    return {l[0] for l in raw}


def review_pr(pr, args, worktree_dir):
    master_rev = fetch_ref(pr["base"]["ref"])
    sh(["git", "worktree", "add", worktree_dir, master_rev])
    master_packages = list_packages(worktree_dir)

    pr_rev = fetch_ref(f"pull/{pr['number']}/head")
    sh(["git", "merge", pr_rev, "-m", "auto merge"], cwd=worktree_dir)

    merged_packages = list_packages(worktree_dir, check_meta=True)

    attrs = differences(master_packages, merged_packages)
    build_in_path(args, attrs, worktree_dir)


def find_nixpkgs_root():
    prefix = ["."]
    release_nix = ["nixos", "release.nix"]
    while True:
        root_path = os.path.join(*prefix)
        release_nix_path = os.path.join(root_path, *release_nix)
        if os.path.exists(release_nix_path):
            return root_path
        if os.path.abspath(root_path) == '/':
            return None
        prefix.append("..")


def main():
    root = find_nixpkgs_root()
    if root is None:
        die("Has to be execute from nixpkgs repository")

    os.chdir(root)

    if len(sys.argv) < 2:
        print(
            f"USAGE: {sys.argv[0]} <pr-number> [NIX-SHELL-OPTIONS...]",
            file=sys.stderr)

    try:
        pr_number = int(sys.argv[1])
    except ValueError:
        print("first argument must be a positive number", file=sys.stderr)
        sys.exit(1)

    api_url = f"https://api.github.com/repos/NixOS/nixpkgs/pulls/{pr_number}"
    pr = json.load(urllib.request.urlopen(api_url))

    git_root = os.path.realpath(".")
    os.makedirs(os.path.join(git_root, ".review"), exist_ok=True)
    worktree_dir = tempfile.mkdtemp(
        prefix=os.path.join(git_root, f".review/pr-{pr['number']}-"))
    try:
        with tempfile.NamedTemporaryFile() as cfg:
            cfg.write(b"pkgs: { allowUnfree = true; }")
            cfg.flush()
            os.environ['NIXPKGS_CONFIG'] = cfg.name
            os.environ['NIX_PATH'] = f"nixpkgs={worktree_dir}"

            review_pr(pr, sys.argv[2:], worktree_dir)
    finally:
        shutil.rmtree(worktree_dir)
        sh(["git", "worktree", "prune"])


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        pass
