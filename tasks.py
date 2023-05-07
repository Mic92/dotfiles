#!/usr/bin/env python3

import json
import os
import socket
import subprocess
import sys
from pathlib import Path
from typing import Any, List

from deploykit import DeployGroup, DeployHost, HostKeyCheck, parse_hosts
from invoke import task

ROOT = Path(__file__).parent.resolve()
os.chdir(ROOT)


def deploy_nixos(hosts: List[DeployHost]) -> None:
    """
    Deploy to all hosts in parallel
    """
    g = DeployGroup(hosts)

    res = subprocess.run(
        ["nix", "flake", "metadata", "--json"],
        check=True,
        text=True,
        stdout=subprocess.PIPE,
    )
    data = json.loads(res.stdout)
    path = data["path"]

    def deploy(h: DeployHost) -> None:
        target = f"{h.user or 'root'}@{h.host}"
        flake_path = h.meta.get("flake_path", "/etc/nixos")
        h.run_local(
            f"rsync --checksum -vaF --delete -e ssh {path}/ {target}:{flake_path}"
        )

        flake_attr = h.meta.get("flake_attr", "")
        if flake_attr:
            flake_attr = "#" + flake_attr
        target_host = h.meta.get("target_host")
        if target_host:
            target_user = h.meta.get("target_user")
            if target_user:
                target_host = f"{target_user}@{target_host}"
        extra_args = h.meta.get("extra_args", [])
        cmd = (
            ["nixos-rebuild", "switch"]
            + extra_args
            + [
                "--fast",
                "--option",
                "keep-going",
                "true",
                "--option",
                "accept-flake-config",
                "true",
                "--build-host",
                "",
                "--flake",
                f"{flake_path}{flake_attr}",
            ]
        )
        if target_host:
            cmd.extend(["--target-host", target_host])
        ret = h.run(cmd, check=False)
        # re-retry switch if the first time fails
        if ret.returncode != 0:
            ret = h.run(cmd)

    g.run_function(deploy)


@task
def update_sops_files(c):
    """
    Update all sops yaml and json files according to .sops.yaml rules
    """
    c.run(
        """
        cd nixos && find . -type f \( -iname '*.enc.json' -o -iname '*.yaml' \) -print0 | \
        xargs -0 -n1 sops updatekeys --yes
"""
    )


@task
def generate_password(c, user="root"):
    """
    Generate password hashes for users i.e. for root in ./hosts/$HOSTNAME.yml
    """
    passw = subprocess.run(
        [
            "nix",
            "run",
            "--inputs-from",
            ".#",
            "nixpkgs#xkcdpass",
            "--",
            "-d-",
            "-n3",
            "-C",
            "capitalize",
        ],
        text=True,
        check=True,
        stdout=subprocess.PIPE,
    ).stdout.strip()
    hash = subprocess.run(
        [
            "nix",
            "run",
            "--inputs-from",
            ".#",
            "nixpkgs#mkpasswd",
            "--",
            "-m",
            "sha-512",
            "-s",
        ],
        text=True,
        check=True,
        stdout=subprocess.PIPE,
        input=passw,
    ).stdout.strip()
    print("# Add the following secrets")
    print(f"{user}-password: {passw}")
    print(f"{user}-password-hash: {hash}")


def get_hosts(hosts: str) -> List[DeployHost]:
    return [DeployHost(h, user="root") for h in hosts.split(",")]


@task
def deploy(c: Any, _hosts: str = "") -> None:
    """
    Deploy to eve, eva and localhost
    """
    eve = DeployHost("eve.i", user="root")
    if _hosts != "":
        hosts = get_hosts(_hosts)
    else:
        hosts = [
            eve,
            DeployHost(
                "localhost",
                user="root",
                forward_agent=True,
            ),
            DeployHost(
                "eve.i",
                user="root",
                forward_agent=True,
                command_prefix="eva.r",
                meta=dict(target_host="eva.i", flake_attr="eva"),
            ),
            DeployHost(
                "eve.i",
                user="root",
                forward_agent=True,
                command_prefix="blob64.r",
                meta=dict(target_host="blob64.r", flake_attr="blob64"),
            ),
        ]
    deploy_nixos(hosts)
    if _hosts == "":
        eve.run("systemctl restart buildbot-master")


@task
def deploy_k3s(c: Any) -> None:
    """
    Deploy k3s cluster to cloudlab
    """
    deploy_nixos(
        [
            DeployHost(
                "node0.nixos-k3s.Serverless-tum.emulab.net",
                user="root",
                meta=dict(flake_attr="cloudlab-k3s-server"),
            ),
            DeployHost(
                "node1.nixos-k3s.Serverless-tum.emulab.net",
                user="root",
                meta=dict(flake_attr="cloudlab-k3s-agent"),
            ),
            DeployHost(
                "node2.nixos-k3s.Serverless-tum.emulab.net",
                user="root",
                meta=dict(flake_attr="cloudlab-k3s-agent"),
            ),
        ]
    )


def try_local(host: str) -> str:
    try:
        socket.gethostbyname(f"{host}.lan")
        return f"{host}.lan"
    except socket.error:
        return f"{host}.r"


@task
def deploy_bernie(c: Any) -> None:
    """
    Deploy to bernie
    """
    deploy_nixos([DeployHost(try_local("bernie"), user="root")])


@task
def deploy_matchbox(c: Any) -> None:
    """
    Deploy to matchbox
    """
    deploy_nixos(
        [
            DeployHost(
                "localhost",
                command_prefix="matchbox.r",
                meta=dict(
                    target_user="root",
                    target_host=try_local("matchbox"),
                    flake_attr="matchbox",
                ),
                user="root",
            )
        ]
    )


@task
def deploy_dotfiles(c: Any) -> None:
    """
    Deploy to dotfiles
    """

    hosts = [
        DeployHost("localhost", meta=dict(flake_attr="desktop")),
        DeployHost("eve.r", meta=dict(flake_attr="eve")),
    ]
    g = DeployGroup(hosts)

    def deploy_homemanager(host: DeployHost) -> None:
        host.run(
            f"""sudo -u joerg zsh <<'EOF'
cd $HOME
source $HOME/.zshrc
homeshick pull
homeshick symlink
homeshick cd dotfiles
nix build --out-link $HOME/.hm-activate ".#hmConfigurations.{host.meta["flake_attr"]}.activation-script"
$HOME/.hm-activate/activate
EOF"""
        )

    g.run_function(deploy_homemanager)


def wait_for_port(host: str, port: int, shutdown: bool = False) -> None:
    import socket
    import time

    while True:
        try:
            with socket.create_connection((host, port), timeout=1):
                if shutdown:
                    time.sleep(1)
                    sys.stdout.write(".")
                    sys.stdout.flush()
                else:
                    break
        except OSError:
            if shutdown:
                break
            else:
                time.sleep(0.01)
                sys.stdout.write(".")
                sys.stdout.flush()


def wait_for_reboot(h: DeployHost) -> None:
    print(f"Wait for {h.host} to shutdown", end="")
    sys.stdout.flush()
    assert h.port is not None, "port is not set"
    wait_for_port(h.host, h.port, shutdown=True)
    print("")

    print(f"Wait for {h.host} to start", end="")
    sys.stdout.flush()
    wait_for_port(h.host, h.port)
    print("")


@task
def add_github_user(c: Any, hosts: str = "", github_user: str = "Mic92") -> None:
    def add_user(h: DeployHost) -> None:
        h.run("mkdir -m700 /root/.ssh")
        out = h.run_local(
            f"curl https://github.com/{github_user}.keys", stdout=subprocess.PIPE
        )
        h.run(
            f"echo '{out.stdout}' >> /root/.ssh/authorized_keys && chmod 700 /root/.ssh/authorized_keys"
        )

    g = parse_hosts(hosts, host_key_check=HostKeyCheck.NONE)
    g.run_function(add_user)


@task
def reboot(c: Any, hosts: str = "") -> None:
    """
    Reboot hosts. example usage: fab --hosts clara.r,donna.r reboot
    """
    deploy_hosts = [DeployHost(h) for h in hosts.split(",")]
    for h in deploy_hosts:
        g = DeployGroup([h])
        g.run("reboot &")

        wait_for_reboot(h)


# curl -L https://github.com/nix-community/nixos-images/releases/download/nixos-unstable/nixos-kexec-installer-x86_64-linux.tar.gz | tar -xzf- -C /root
# /root/kexec/run
# zpool import -a; zfs load-key -a; mount -t zfs zroot/root/nixos /mnt; mount /dev/nvme
# nixos-install --flake github:mic92/dotfiles#eve


@task
def cleanup_gcroots(c: Any, hosts: str = "") -> None:
    deploy_hosts = [DeployHost(h) for h in hosts.split(",")]
    for h in deploy_hosts:
        g = DeployGroup([h])
        g.run("find /nix/var/nix/gcroots/auto -type s -delete")
        g.run("systemctl restart nix-gc")
