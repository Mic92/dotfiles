import contextlib
import os
import socket
import subprocess
import sys
import time
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Any

from deploykit import DeployGroup, DeployHost, HostKeyCheck, parse_hosts
from invoke import task

ROOT = Path(__file__).parent.resolve()
os.chdir(ROOT)


@task
def decrypt_eve(_c: Any) -> None:
    """Decrypt secrets"""
    eve_initrd = DeployHost("95.217.199.121", user="root", port=2222)
    pw = subprocess.run(
        ["rbw", "get", "zfs encryption"],
        text=True,
        check=True,
        stdout=subprocess.PIPE,
    ).stdout.strip()
    # ssh may timeout, so we try multiple times
    for _ in range(3):
        with contextlib.suppress(subprocess.CalledProcessError):
            eve_initrd.run("true")

    eve_initrd.run(f'echo "{pw}" | systemd-tty-ask-password-agent')


@task
def reboot_and_decrypt_eve(c: Any) -> None:
    """Reboot hosts and decrypt secrets"""
    eve = DeployHost("95.217.199.121", user="root")
    eve.run("reboot &")
    wait_for_reboot(eve)
    decrypt_eve(c)


@task
def deploy(c: Any, _hosts: str = "") -> None:
    """Deploy to eve, eva, blob64 and localhost"""
    c.run("clan machines update")


def try_local(host: str) -> str:
    try:
        socket.gethostbyname(f"{host}.lan")
    except OSError:
        return f"{host}.r"
    else:
        return f"{host}.lan"


@task
def deploy_bernie(c: Any) -> None:
    """Deploy to bernie"""
    c.run("clan machines update bernie")


@task
def deploy_matchbox(c: Any) -> None:
    """Deploy to matchbox"""
    c.run("clan machines update matchbox")


@task
def deploy_dotfiles(c: Any) -> None:
    """Deploy to dotfiles"""
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
EOF""",
        )

    g.run_function(deploy_homemanager)


@task
def install_machine(c: Any, flake_attr: str, hostname: str) -> None:
    """Install nixos on a machine"""
    ask = input(f"Install {hostname} with {flake_attr}? [y/N] ")
    if ask != "y":
        return
    with TemporaryDirectory() as d:
        root = Path(d) / "root"
        root.mkdir(parents=True, exist_ok=True)
        root.chmod(0o755)
        host_key = root / "etc/ssh/ssh_host_ed25519_key"
        host_key.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run(
            [
                "sops",
                "--extract",
                '["ssh_host_ed25519_key"]',
                "-d",
                f"nixos/{flake_attr}/secrets/secrets.yaml",
            ],
            check=True,
            stdout=host_key.open("w"),
        )
        c.run(
            f"nix run github:numtide/nixos-anywhere -- {hostname} --debug --flake .#{flake_attr} --extra-files {root}",
            echo=True,
        )


def wait_for_host(h: DeployHost, shutdown: bool) -> None:
    while True:
        res = subprocess.run(
            ["ping", "-q", "-c", "1", "-w", "2", h.host],
            stdout=subprocess.DEVNULL,
            check=False,
        )
        if (shutdown and res.returncode == 1) or (not shutdown and res.returncode == 0):
            break
        time.sleep(1)
        sys.stdout.write(".")
        sys.stdout.flush()


@task
def generate_facter_json(
    c: Any, hosts: str, facter: str = "github:numtide/nixos-facter"
) -> None:
    """
    Deploy to servers
    """

    def deploy(h: DeployHost) -> None:
        ret = h.run(
            ["nix", "run", "--refresh", facter],
            stdout=subprocess.PIPE,
        )
        name = h.host.split(".")[0]
        path = ROOT / "nixos" / name / "facter.json"
        path.write_text(ret.stdout)

    host_list = hosts.split(",")
    g = DeployGroup([DeployHost(h, user="root") for h in host_list])
    g.run_function(deploy)


def wait_for_reboot(h: DeployHost) -> None:
    print(f"Wait for {h.host} to shutdown", end="")
    sys.stdout.flush()
    wait_for_host(h, shutdown=True)
    print()

    print(f"Wait for {h.host} to start", end="")
    sys.stdout.flush()
    wait_for_host(h, shutdown=True)
    print()


@task
def add_github_user(c: Any, hosts: str = "", github_user: str = "Mic92") -> None:
    def add_user(h: DeployHost) -> None:
        h.run("mkdir -m700 /root/.ssh")
        out = h.run_local(
            f"curl https://github.com/{github_user}.keys",
            stdout=subprocess.PIPE,
        )
        h.run(
            f"echo '{out.stdout}' >> /root/.ssh/authorized_keys && chmod 700 /root/.ssh/authorized_keys",
        )

    g = parse_hosts(hosts, host_key_check=HostKeyCheck.NONE)
    g.run_function(add_user)


@task
def update_flakes(c: Any) -> None:
    c.run(
        "TMPDIR=/run/user/$(id -u)/ fast-flake-update --rev origin/main nixpkgs ~/git/nixpkgs",
        echo=True,
    )
    c.run("nix flake update --commit-lock-file", echo=True)


@task
def reboot(c: Any, hosts: str) -> None:
    """Reboot hosts. example usage: fab --hosts clara.r,donna.r reboot"""
    deploy_hosts = [DeployHost(h) for h in hosts.split(",")]
    for h in deploy_hosts:
        g = DeployGroup([h])
        g.run("reboot &")

        wait_for_reboot(h)


@task
def kexec_installer(c: Any, hosts: str) -> None:
    """Kexec into nixos installer, i.e. inv kexec-installer --hosts root@95.217.199.121"""

    def do_kexec(h: DeployHost) -> None:
        h.run(
            "curl -L https://github.com/nix-community/nixos-images/releases/download/nixos-unstable/nixos-kexec-installer-noninteractive-x86_64-linux.tar.gz | tar -xzf- -C /root",
        )
        h.run("/root/kexec/run")
        wait_for_reboot(h)

    g = parse_hosts(hosts, host_key_check=HostKeyCheck.NONE)
    g.run_function(do_kexec)


@task
def disko_mount_from_recovery(c: Any, host: str, flake: str) -> None:
    """Mount the system disk from a recovery system, i.e. inv disko-mount-from-recovery --host root@eve.i --flake github:mic92/dotfiles#eve"""
    h = DeployHost(host)
    h.run(
        f"""nix --extra-experimental-features "nix-command flakes" shell nixpkgs#git -c nix run --extra-experimental-features "nix-command flakes" github:nix-community/disko -- --flake {flake} --mode mount""",
    )


@task
def boot_eve_into_recovery(c: Any) -> None:
    """Mount the system disk from a recovery system, i.e. inv disko-mount-from-recovery --host root@eve.i --flake github:mic92/dotfiles#eve"""
    eve_hostname = "root@95.217.199.121"
    host = parse_hosts(eve_hostname).hosts[0]
    kexec_installer(c, hosts=eve_hostname)
    pw = subprocess.run(
        ["rbw", "get", "zfs encryption"],
        text=True,
        check=True,
        stdout=subprocess.PIPE,
    ).stdout.strip()
    # FIXME: disko does not support interactive zfs load-key:
    host.run(f'zpool import -a; echo "{pw}" | zfs load-key -a')
    disko_mount_from_recovery(c, host=eve_hostname, flake="github:mic92/dotfiles#eve")


@task
def unmount_from_recovery(c: Any, hosts: str, flake: str) -> None:
    """Unmount the system disk from a recovery system, i.e. inv unmount-from-recovery --host root@eve.i"""
    g = DeployGroup([DeployHost(h) for h in hosts.split(",")])
    g.run("umount -R /mnt")
    g.run("zpool export -a")


@task
def cleanup_gcroots(c: Any, hosts: str) -> None:
    deploy_hosts = [DeployHost(h) for h in hosts.split(",")]
    for h in deploy_hosts:
        g = DeployGroup([h])
        g.run("find /nix/var/nix/gcroots/auto -type s -delete")
        g.run("systemctl restart nix-gc")
