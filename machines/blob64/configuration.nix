{
  pkgs,
  self,
  config,
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
    ./modules/photoprism.nix
    ./modules/phantun.nix
    ./modules/pinchflat.nix
    ./modules/ghorg.nix
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

  # GitHub/Gitea repository backups
  services.ghorg.backups = {
    mic92 = {
      target = "Mic92";
      cloneType = "user";
      tokenFile = config.clan.core.vars.generators.ghorg-github-token.files.token.path;
    };
    clan = {
      target = "clan";
      scm = "gitea";
      baseUrl = "https://git.clan.lol";
      tokenFile = config.clan.core.vars.generators.ghorg-gitea-token.files.token.path;
    };
    nix-community = {
      target = "nix-community";
      tokenFile = config.clan.core.vars.generators.ghorg-github-token.files.token.path;
      descriptionFilter = "Mic92";
    };
    numtide = {
      target = "numtide";
      tokenFile = config.clan.core.vars.generators.ghorg-github-token.files.token.path;
      matchRegex = "^(llm-agents\\.nix|treefmt|treefmt-nix)$";
    };
    tum-dse = {
      target = "TUM-DSE";
      tokenFile = config.clan.core.vars.generators.ghorg-github-token.files.token.path;
      matchRegex = "^doctor-cluster-config$";
    };
    mic92-forks = {
      target = "Mic92";
      cloneType = "user";
      tokenFile = config.clan.core.vars.generators.ghorg-github-token.files.token.path;
      matchRegex = "^(nix-1|nixpkgs)$";
      skipForks = false;
    };
  };
}
