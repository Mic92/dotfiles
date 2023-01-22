{ config, pkgs, lib, ... }:
{
  imports = [
    ./modules/borgbackup-repos
    ../modules/users.nix
    ../modules/minimal-docs.nix
    ../modules/networkd.nix
    ../modules/ip-update.nix
    #../modules/tailscale.nix
    ./hardware-configuration.nix
  ];

  sops.defaultSopsFile = ./secrets/secrets.yaml;
  system.stateVersion = "22.11";

  sops.secrets.root-password-hash.neededForUsers = true;
  users.users.root.passwordFile = config.sops.secrets.root-password-hash.path;

  # Fan speed adjustment
  systemd.services.fans =
    let
      fancontrol = pkgs.rustPlatform.buildRustPackage {
        name = "fancontrol";
        src = ./fancontrol;
        cargoLock.lockFile = ./fancontrol/Cargo.lock;
      };
    in
    {
      wantedBy = [ "multi-user.target" ];
      serviceConfig.ExecStart = lib.getExe fancontrol;
      serviceConfig.Restart = "always";
    };

  services.openssh.enable = true;

  networking.hostName = "blob64";

  environment.systemPackages = with pkgs; [
    vim
    ethtool
    iperf
    parted
    fio
    wget
    htop
    tcpdump
    nmap
    python3
    tmux
  ];

  systemd.network.networks."10-uplink" = {
    matchConfig.Type = "ether";
    networkConfig.DHCP = "yes";
  };

  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "ac174b52";
}
