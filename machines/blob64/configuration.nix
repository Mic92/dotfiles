{
  pkgs,
  self,
  ...
}:
{
  imports = [
    self.nixosModules.default
    self.inputs.srvos.nixosModules.server
    ../../nixosModules/users.nix
    ../../nixosModules/minimal-docs.nix
    ../../nixosModules/networkd.nix
    ../../nixosModules/ip-update.nix
    ../../nixosModules/packages.nix
    ../../nixosModules/hyprspace.nix
    ./modules/photoprism.nix
    ./modules/phantun.nix
  ];

  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.aarch64-linux;

  networking.hostName = "blob64";

  clan.core.networking.buildHost = "root@eve.i";

  system.stateVersion = "23.11";

  # Fan speed adjustment
  systemd.services.fans =
    let
      fancontrol = pkgs.rustPlatform.buildRustPackage {
        name = "fancontrol";
        src = ./fancontrol;
        fetchCargoVendor = true;
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
