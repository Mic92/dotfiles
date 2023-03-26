{ pkgs, ... }: {
  nixpkgs.localSystem.system = "aarch64-linux";

  imports = [
    ../modules/users.nix
    ../modules/mosh.nix
    ../modules/sshd/tor.nix
    ../modules/promtail.nix

    ./hardware-configuration.nix
    ./modules/borgbackup.nix
    ./modules/samba.nix
    ./modules/rsyncd.nix
    ./modules/sops.nix
  ];

  documentation.enable = false;

  networking.hostName = "matchbox";

  time.timeZone = "UTC";

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

  system.stateVersion = "18.09";
  networking.dhcpcd.enable = true;
  services.resolved.enable = false;
}
