{ pkgs, ... }:
{
  imports = [
    ./modules/borgbackup-repos
    ../modules/users.nix
    ../modules/minimal-docs.nix
    ../modules/networkd.nix
    ../modules/ip-update.nix
    ../modules/packages.nix
    ./modules/photoprism.nix
    ./hardware-configuration.nix
  ];

  networking.hostName = "blob64";

  clan.networking.buildHost = "root@eve.i";

  system.stateVersion = "23.11";

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
      serviceConfig.ExecStart = "${fancontrol}/bin/fancontrol";
      serviceConfig.Restart = "always";
    };

  services.openssh.enable = true;

  systemd.network.networks."10-uplink" = {
    matchConfig.Type = "ether";
    networkConfig.DHCP = "yes";
  };

  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "ac174b52";

  services.zerotierone.localConf.settings.forceTcpRelay = true;
}
