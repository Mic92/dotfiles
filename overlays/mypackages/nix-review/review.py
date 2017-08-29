#!/usr/bin/env python
# this script requires at least python 3.6!
import tempfile
import subprocess
import os
import sys
import shutil
import xml.etree.ElementTree as ET
import multiprocessing

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

    canonical_path = str(os.path.realpath(path))
    result_dir = tempfile.mkdtemp(prefix='nox-review-')
    print('Building in {}: {}'.format(result_dir, ' '.join(attrs)))
    command = [
            'nix-shell',
            '--keep-going',
            f"-j{multiprocessing.cpu_count()}",
            "--option", "build-use-sandbox", "true" # only matters for single-user nix
            ] + args
    for a in attrs:
        # add option to opt out builds
        #if 'libreoffice' in a or 'chromium' in a:
        #    continue
        command.append('-p')
        command.append(a)

    try:
        print(" ".join(command))
        sh(command, cwd=result_dir)
    except subprocess.CalledProcessError:
        die('The invocation of "{}" failed'.format(' '.join(command)))

def list_packages(path, check_meta=False):
    cmd = ['nix-env', '-f', path, '-qaP', '--xml', '--out-path', '--show-trace']
    if check_meta:
        cmd.append("--meta")
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    context = ET.iterparse(process.stdout, events=("start",))
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
    sh(["git", "fetch", "https://github.com/NixOS/nixpkgs", ref])
    o = subprocess.check_output(["git", "rev-parse", "--verify", "FETCH_HEAD"])
    return o.strip().decode("utf-8")

def differences(old, new):
    raw = new - old
    return {l[0] for l in raw}

def review_pr(pr, args, worktree_dir):
    master_rev = fetch_ref("master")
    sh(["git", "worktree", "add", worktree_dir, master_rev])
    master_packages = list_packages(worktree_dir)

    pr_rev = fetch_ref(f"pull/{pr}/head")
    sh(["git", "merge", pr_rev, "-m", "auto merge"], cwd=worktree_dir)

    merged_packages = list_packages(worktree_dir, check_meta=True)

    attrs = differences(master_packages, merged_packages)
    build_in_path(args, attrs, worktree_dir)

def main():
    if not os.path.exists("nixos/release.nix"):
        die("Has to be execute from nixpkgs repository")

    pr = int(sys.argv[1])

    git_root = os.path.realpath(".")
    os.makedirs(os.path.join(git_root, ".review"), exist_ok=True)
    worktree_dir = tempfile.mkdtemp(prefix=os.path.join(git_root, f".review/pr-{pr}-"))
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
