# To deploy as a systemd-nspawn container on a new host:
# $ nix run github:nix-community/nixos-generators -- --format lxc --flake '.#eva'
# $ cat /etc/systemd/nspawn/eva.nspawn
# [Exec]
# Capability = all
# PrivateUsers = no
#
# [Network]
# VirtualEthernet = no
# $ mkdir -p /var/lib/machines/eva
# $ tar -C /var/lib/machines/eva -xf nixos-system-x86_64-linux.tar
# # provision /etc/os-release, the command will fail but systemd-nspawn will be
# # able to boot our directory afterwards
# $ unshare --mount -- chroot /var/lib/machines/eva /sbin/init
# $ systemd-nspawn --capability=CAP_NET_ADMIN -D /var/lib/machines/eva -b
# or
# $ machinectl start eva
{ lib, pkgs, ... }: {
  networking.hostName = "eva";

  time.timeZone = "UTC";

  programs.vim.defaultEditor = true;

  imports = [
    ./modules/borgbackup-repos
    ./modules/buildbot.nix
    ./modules/go-neb.nix
    ./modules/hardware-configuration.nix
    ./modules/prometheus
    ./modules/loki.nix
    ./modules/nginx.nix
    ./modules/sshd.nix
    ./modules/sops.nix

    ../modules/mosh.nix
    ../modules/iperf.nix
    ../modules/openldap/replica.nix
    ../modules/promtail.nix
    ../modules/tracing.nix
    ../modules/users.nix
  ];

  # this is a container, and we leave the firewall to the host
  networking.firewall.enable = lib.mkForce false;

  # let the host manage these
  systemd.network.networks."ethernet".extraConfig = ''
    [Match]
    Type = ether

    [Link]
    Unmanaged = yes
  '';

  services.resolved.enable = false;

  # breaks loki
  networking.usePredictableInterfaceNames = false;

  # lxc-container.nix sets this
  documentation.enable = lib.mkForce false;

  environment.systemPackages = [
    pkgs.htop
    pkgs.tmux
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "20.03";
}
