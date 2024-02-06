{ pkgs, ... }: {
  nixpkgs.localSystem.system = "aarch64-linux";

  imports = [
    ./hardware-configuration.nix
    ./modules/samba.nix
    ./modules/rsyncd.nix
    ./modules/photoprism.nix

    ../modules/borgbackup.nix
    ../modules/users.nix
    ../modules/mosh.nix
    ../modules/sshd/tor.nix
    ../modules/promtail.nix
  ];

  documentation.enable = false;
  clan.deployment.requireExplicitUpdate = true;

  networking.hostName = "matchbox";

  time.timeZone = "UTC";

  services.getty.autologinUser = "root";

  environment.systemPackages = with pkgs; [
    tmux
    htop
    iotop
    tcpdump
    strace
    ethtool
    usbutils
    bandwhich
    vim
  ];

  systemd.services.update-prefetch.enable = false;

  system.stateVersion = "23.11";

  networking.dhcpcd.enable = false;
  systemd.network.networks.ethernet.extraConfig = ''
    [Match]
    Type = ether

    [Network]
    DHCP = both
    LLMNR = true
    IPv4LL = true
    LLDP = true
    IPv6AcceptRA = true
    IPv6Token = ::fd87:20d6:a932:6605

    [DHCP]
    UseHostname = false
    RouteMetric = 512
  '';
  services.resolved.enable = false;
}
