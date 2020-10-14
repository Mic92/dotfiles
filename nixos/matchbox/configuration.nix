{ pkgs, ... }: {

  fileSystems = {
    "/mnt/hdd" = {
       device = "UUID=1d377ab7-65ca-492d-9ea4-620034230192";
       fsType = "ext4";
       options = [ "defaults" "nofail" "x-systemd.device-timeouts=2" ];
    };
    "/mnt/backup" = {
      device = "UUID=11ac8bec-aef1-45ca-a530-2115d403ce53";
      fsType = "ext4";
      options = [ "defaults" "nofail" "x-systemd.device-timeouts=2" ];
    };
  };

  imports = [
    ../modules/users.nix
    ../modules/mosh.nix
    ../modules/tor-ssh.nix
    ../modules/rpi3.nix

    ./modules/borgbackup.nix
    ./modules/samba.nix
    ./modules/rsyncd.nix
  ];

  networking.retiolum = {
    ipv4 = "10.243.29.176";
    ipv6 = "42:0:3c46:6745:adf4:a844:26c4:bf91";
  };

  networking.hostName = "matchbox";

  environment.systemPackages = with pkgs; [
    tmux
    htop
    iotop
    tcpdump
    strace
    ethtool
    usbutils
    vim
  ];

  system.stateVersion = "18.09";
  networking.dhcpcd.enable = true;
}
