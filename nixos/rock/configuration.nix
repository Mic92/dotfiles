{ pkgs, ... }: {
  nixpkgs.localSystem.system = "aarch64-linux";

  imports = [
    ./hardware-configuration.nix

    ./modules/bme680-mqtt.nix
    ./modules/sops.nix
    ./modules/vlc-telnet.nix

    ../modules/hass-agent.nix
    ../modules/users.nix
    ../modules/ip-update.nix
    ../modules/samba-dl.nix
    ../modules/mosh.nix
    ../modules/networkd.nix
    ../modules/mosh.nix
    ../modules/promtail.nix
    ../modules/sshd/tor.nix
    #./modules/mycroft.nix
  ];

  programs.bcc.enable = true;

  documentation.enable = false;

  # Use the extlinux boot loader. (NixOS wants to enable GRUB by default)
  boot.loader.grub.enable = false;
  # Enables the generation of /boot/extlinux/extlinux.conf
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "rock";

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
