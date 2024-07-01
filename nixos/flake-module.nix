{ self, inputs, ... }:
{
  flake.nixosModules.default =
    { config, lib, ... }:
    {
      srvos.flake = self;
      documentation.info.enable = false;
      services.envfs.enable = true;
      clan.networking.targetHost = lib.mkDefault "root@${config.networking.hostName}.r";

      imports = [
        ./modules/nix-path.nix
        ./modules/pinned-registry.nix
        ./modules/acme.nix
        ./modules/nix-daemon.nix
        ./modules/minimal-docs.nix
        ./modules/i18n.nix
        ./modules/sshd
        ./modules/zfs.nix

        inputs.clan-core.clanModules.sshd
        inputs.clan-core.clanModules.borgbackup

        inputs.srvos.nixosModules.common
        inputs.srvos.nixosModules.mixins-telegraf
        inputs.srvos.nixosModules.mixins-nix-experimental
        { networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 9273 ]; }
        inputs.srvos.nixosModules.mixins-trusted-nix-caches

        ./modules/retiolum.nix
        ./modules/update-prefetch.nix
        inputs.retiolum.nixosModules.retiolum
        inputs.retiolum.nixosModules.ca

        ./modules/zerotier.nix
        inputs.nether.nixosModules.hosts
      ];
    };
  clan = {
    meta.name = "mic92";
    directory = self;
    specialArgs = {
      self = self;
      inputs = inputs;
    };
    pkgsForSystem = system: inputs.nixpkgs.legacyPackages.${system};

    machines = {
      bernie = {
        nixpkgs.pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
        imports = [
          ./bernie/configuration.nix
          self.nixosModules.default
          inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x13
          inputs.home-manager.nixosModules.home-manager
          inputs.srvos.nixosModules.desktop
        ];
      };

      turingmachine = {
        nixpkgs.pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
        imports = [
          ./turingmachine/configuration.nix
          self.nixosModules.default
          inputs.nixos-hardware.nixosModules.framework-13th-gen-intel
          inputs.nix-index-database.nixosModules.nix-index
          inputs.disko.nixosModules.disko
          inputs.clan-core.clanModules.localbackup
          inputs.agenix.nixosModules.default

          #inputs.spora.nixosModules.spora

          { programs.nix-index-database.comma.enable = true; }
          inputs.srvos.nixosModules.desktop

          inputs.lanzaboote.nixosModules.lanzaboote

          # For testing systemd
          #({pkgs, ...}: {
          #  #systemd.package = self.inputs.nixpkgs-systemd.legacyPackages.${pkgs.system}.systemd;
          #  systemd.package = pkgs.systemd.overrideAttrs (old: {
          #    src = self.inputs.systemd;
          #  });
          #})
        ];
      };
      eve = {
        nixpkgs.pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
        imports = [
          ./eve/configuration.nix
          self.nixosModules.default
          inputs.srvos.nixosModules.server
          inputs.srvos.nixosModules.mixins-nginx
          inputs.srvos.nixosModules.hardware-hetzner-online-amd
          inputs.buildbot-nix.nixosModules.buildbot-worker
          inputs.buildbot-nix.nixosModules.buildbot-master
          inputs.disko.nixosModules.disko
          #inputs.nixos-wiki.nixosModules.nixos-wiki
          #inputs.nixos-wiki.nixosModules.nixos-wiki-backup
        ];
      };

      eva = {
        nixpkgs.pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
        imports = [
          ./eva/configuration.nix
          self.nixosModules.default
          inputs.srvos.nixosModules.server
          inputs.srvos.nixosModules.mixins-nginx
          inputs.srvos.nixosModules.mixins-systemd-boot
          inputs.srvos.nixosModules.roles-prometheus
          inputs.disko.nixosModules.disko

          #inputs.spora.nixosModules.spora
        ];
      };

      blob64 = {
        nixpkgs.pkgs = inputs.nixpkgs.legacyPackages.aarch64-linux;
        imports = [
          ./blob64/configuration.nix
          self.nixosModules.default
          inputs.srvos.nixosModules.server
        ];
      };

      matchbox = {
        nixpkgs.pkgs = inputs.nixpkgs.legacyPackages.aarch64-linux;
        imports = [
          ./matchbox/configuration.nix
          self.nixosModules.default
          inputs.srvos.nixosModules.server
        ];
      };
    };
  };
}
