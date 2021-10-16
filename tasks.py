#!/usr/bin/env python3

from invoke import task

import sys
from typing import List, Dict, Tuple, IO, Iterator, Optional
from contextlib import contextmanager
from deploy_nixos import DeployHost, DeployGroup


def deploy_nixos(hosts: List[DeployHost]) -> None:
    """
    deploy to all hosts in parallel
    """
    g = DeployGroup(hosts)
    g.run_local(
        "rsync --exclude='.git/' -vaF --delete -e ssh . $SSH_USER@$SSH_HOST:/etc/nixos",
    )

    g.run(
        "a=$FLAKE_ATTR;"
        "a=${a:-$(< /proc/sys/kernel/hostname)}; "
        "nixos-rebuild switch --build-host localhost --target-host $TARGET_HOST --flake /etc/nixos#$a"
    )


@task
def deploy(c):
    """
    Deploy to eve, eva and localhost
    """
    deploy_nixos(
        [
            DeployHost("eve.r"),
            DeployHost("localhost"),
            DeployHost(
                "eve.r", target_host="eva.r", flake_attr="eva", forward_agent=True
            ),
        ]
    )


@task
def deploy_bernie(c):
    """
    Deploy to bernie
    """
    deploy_nixos([DeployHost("bernie.r")])


@task
def deploy_matchbox(c):
    """
    Deploy to matchbox
    """
    deploy_nixos(
        [DeployHost("localhost", target_host="matchbox.r", flake_attr="matchbox")]
    )


@task
def deploy_rock(c):
    """
    Deploy to matchbox
    """
    deploy_nixos([DeployHost("localhost", target_host="rock.r", flake_attr="rock")])


@task
def deploy_dotfiles(c):
    """
    Deploy to dotfiles
    """

    hosts = [
        DeployHost("localhost", flake_attr="desktop"),
        DeployHost("eve.r", flake_attr="eve"),
    ]
    g = DeployGroup(hosts)
    g.run(
        """sudo -u joerg zsh <<'EOF'
cd $HOME
source $HOME/.zshrc
homeshick pull
homeshick symlink
homeshick cd dotfiles
nix build --out-link $HOME/.hm-activate ".#hmConfigurations.$FLAKE_ATTR.activation-script"
$HOME/.hm-activate/activate
EOF"""
    )


def wait_for_port(host: str, port: int, shutdown: bool = False) -> None:
    import socket, time

    while True:
        try:
            with socket.create_connection((host, port), timeout=1):
                if shutdown:
                    time.sleep(1)
                    sys.stdout.write(".")
                    sys.stdout.flush()
                else:
                    break
        except OSError as ex:
            if shutdown:
                break
            else:
                time.sleep(0.01)
                sys.stdout.write(".")
                sys.stdout.flush()


@task
def reboot(c, hosts=""):
    """
    Reboot hosts. example usage: fab --hosts clara.r,donna.r reboot
    """
    deploy_hosts = [DeployHost(h) for h in hosts.split(",")]
    for h in deploy_hosts:
        g = DeployGroup([h])
        g.run("reboot &")

        print(f"Wait for {h.host} to shutdown", end="")
        sys.stdout.flush()
        wait_for_port(h.host, h.port, shutdown=True)
        print("")

        print(f"Wait for {h.host} to start", end="")
        sys.stdout.flush()
        wait_for_port(h.host, h.port)
        print("")


@task
def cleanup_gcroots(c, hosts=""):
    deploy_hosts = [DeployHost(h) for h in hosts.split(",")]
    for h in deploy_hosts:
        g = DeployGroup([h])
        g.run("find /nix/var/nix/gcroots/auto -type s -delete")
        g.run("systemctl restart nix-gc")
