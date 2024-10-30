#!/usr/bin/env python3
import contextlib
import os
import subprocess
import sys


def main() -> None:
    wanted = set(map(int, sys.argv[1:]))
    ret = subprocess.run(["mas", "list"], stdout=subprocess.PIPE, check=True)
    installed_ids = set()
    for line in ret.stdout.splitlines():
        columns = line.split()
        with contextlib.suppress(ValueError):
            installed_ids.add(int(columns[0]))
    core_apps = {
        408981434,  # iMovie
        409183694,  # Keynote
        682658836,  # GarageBand
        409201541,  # Pages
        409203825,  # Numbers
    }
    unwanted = installed_ids - wanted - core_apps
    missing = wanted - installed_ids

    maybe_sudo = ["sudo"]
    if os.geteuid() == 0:
        maybe_sudo = []

    if unwanted:
        print(f"Remove the following apps: {' '.join(map(str, unwanted))}")
        for store_id in unwanted:
            subprocess.run([*maybe_sudo, "mas", "uninstall", str(store_id)], check=True)

    if missing:
        print(f"Install the following apps: {' '.join(map(str, missing))}")
        for store_id in missing:
            subprocess.run(["mas", "install", str(store_id)], check=True)


if __name__ == "__main__":
    main()
