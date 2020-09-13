{ config, pkgs, lib, ... }: {
  nixpkgs.localSystem.system = "aarch64-linux";

  imports = [
    ./modules/kodi.nix
    ./hardware-configuration.nix
    ../modules/users.nix
    ../modules/nfs-dl.nix
    ../modules/mosh.nix
    ../modules/tor-ssh.nix
    ../modules/networkd.nix
    ../modules/nix-daemon.nix
  ];

  # Use the extlinux boot loader. (NixOS wants to enable GRUB by default)
  boot.loader.grub.enable = false;
  # Enables the generation of /boot/extlinux/extlinux.conf
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "rock";

  networking.retiolum = {
    ipv4 = "10.243.29.171";
    ipv6 = "42:4992:6a6d:700::2";
  };

  systemd.services.netdata = {
    path = with pkgs; [ python3 ];
  };

  systemd.network.networks = {
    ethernet.extraConfig = ''
      [Match]
      Name=eth0

      [Network]
      DHCP=both
      LLMNR=true
      IPv4LL=true
      LLDP=true
      IPv6AcceptRA=true
      IPv6Token=::fd87:20d6:a932:6605

      [DHCP]
      UseHostname=false
      RouteMetric=512
    '';
  };

  environment.systemPackages = with pkgs; [
    tmux
    htop
    iotop
    tcpdump
    strace
    ethtool
  ];

  time.timeZone = "Europe/Berlin";

  system.stateVersion = "18.03";
}
