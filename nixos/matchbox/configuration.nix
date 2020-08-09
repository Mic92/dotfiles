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
    ../modules/retiolum.nix
    ../modules/mosh.nix
    ../modules/tor-ssh.nix
    ../modules/networkd.nix
    ../modules/netdata
    ../modules/rpi3.nix
    ../modules/secrets.nix

    ./modules/borgbackup.nix
    ./modules/samba.nix
    ./modules/cups.nix
    ./modules/network.nix
    ./modules/cloud-print-connector.nix
    ./modules/rsyncd.nix
  ];

  networking.hostName = "matchbox";

  systemd.services.netdata.path = with pkgs; [
    python3
  ];

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
}
