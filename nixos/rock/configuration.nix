{ config, pkgs, lib, ... }: {
  nixpkgs.localSystem.system = "aarch64-linux";

  imports = [
    ./modules/bme680-mqtt.nix
    ./modules/sops.nix
    ./modules/loki.nix
    ./hardware-configuration.nix

    ../modules/users.nix
    ../modules/samba-dl.nix
    ../modules/mosh.nix
    ../modules/telegraf.nix
    ../modules/tor-ssh.nix
    ../modules/networkd.nix
    ../modules/openldap/replica.nix
    ../modules/promtail.nix
  ];

  services.openssh.extraConfig = ''
    HostCertificate ${./rock-cert.pub}
  '';

  programs.bcc.enable = true;

  documentation.enable = false;
  boot.kernelPackages = pkgs.nur.repos.mic92.linuxPackages_ayufan;

  # Use the extlinux boot loader. (NixOS wants to enable GRUB by default)
  boot.loader.grub.enable = false;
  # Enables the generation of /boot/extlinux/extlinux.conf
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "rock";

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
