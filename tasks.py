#!/usr/bin/env python3

from invoke import task

import sys
from typing import List
from deploy_nixos import DeployHost, DeployGroup, parse_hosts, HostKeyCheck


def deploy_nixos(hosts: List[DeployHost]) -> None:
    """
    Deploy to all hosts in parallel
    """
    g = DeployGroup(hosts)

    def deploy(h: DeployHost) -> None:
        h.run_local(
            f"rsync --exclude='.git/' -vaF --delete -e ssh . {h.user}@{h.host}:/etc/nixos",
        )

        flake_path = "/etc/nixos"
        flake_attr = h.meta.get("flake_attr")
        if flake_attr:
            flake_path += "#" + flake_attr
        target_host = h.meta.get("target_host", "localhost")
        h.run(
            f"nixos-rebuild switch --build-host localhost --target-host {target_host} --flake {flake_path}"
        )

    g.run_function(deploy)


@task
def deploy(c):
    """
    Deploy to eve, eva and localhost
    """
    deploy_nixos(
        [
            DeployHost("eve.i"),
            DeployHost("localhost"),
            DeployHost(
                "eve.i",
                forward_agent=True,
                command_prefix="eva.r",
                meta=dict(target_host="eva.r", flake_attr="eva"),
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
        [
            DeployHost(
                "localhost",
                command_prefix="matchbox.r",
                meta=dict(target_host="matchbox.r", flake_attr="matchbox"),
            )
        ]
    )


@task
def deploy_rock(c):
    """
    Deploy to matchbox
    """
    deploy_nixos(
        [DeployHost("localhost", meta=dict(target_host="rock.r", flake_attr="rock"))]
    )


@task
def deploy_dotfiles(c):
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


def wait_for_reboot(h: DeployHost):
    print(f"Wait for {h.host} to shutdown", end="")
    sys.stdout.flush()
    wait_for_port(h.host, h.port, shutdown=True)
    print("")

    print(f"Wait for {h.host} to start", end="")
    sys.stdout.flush()
    wait_for_port(h.host, h.port)
    print("")


@task
def kexec_nixos(c, hosts=""):
    """
    Boot into nixos via kexec: inv kexec-nixos --hosts
    """

    def kexec(h: DeployHost) -> None:
        # in case the machine is still booting
        print(f"Wait for {h.host} to start", end="")
        sys.stdout.flush()
        wait_for_port(h.host, h.port)

        url = "https://boot.thalheim.io/kexec-image-$(uname -m).tar.xz"
        h.run(
            f"(wget {url} -qO- || curl {url}) | tar -C / -xJf -",
            become_root=True
        )
        h.run(f"/kexec_nixos", become_root=True)

        print(f"Wait for {h.host} to start", end="")
        sys.stdout.flush()
        wait_for_port(h.host, h.port)
        print("")

    # avoid importing temporary ssh keys
    g = parse_hosts(hosts, host_key_check=HostKeyCheck.NONE)
    g.run_function(kexec)


@task
def cloudlab_install(c, disk="/dev/sda", hosts=""):
    def install(h: DeployHost) -> None:
        h.run_local(
            f"rsync --exclude='.git/' -aF --delete -e ssh . {h.user}@{h.host}:/etc/nixos",
        )
        h.run(f"/etc/nixos/nixos/images/cloudlab/install.sh /etc/nixos#cloudlab-node")
    g = parse_hosts(hosts, host_key_check=HostKeyCheck.TOFU)
    g.run_function(install)


@task
def reboot(c, hosts=""):
    """
    Reboot hosts. example usage: fab --hosts clara.r,donna.r reboot
    """
    deploy_hosts = [DeployHost(h) for h in hosts.split(",")]
    for h in deploy_hosts:
        g = DeployGroup([h])
        g.run("reboot &")

        wait_for_reboot(h)


@task
def cleanup_gcroots(c, hosts=""):
    deploy_hosts = [DeployHost(h) for h in hosts.split(",")]
    for h in deploy_hosts:
        g = DeployGroup([h])
        g.run("find /nix/var/nix/gcroots/auto -type s -delete")
        g.run("systemctl restart nix-gc")
