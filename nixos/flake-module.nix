{ self, inputs, ... }:
let
  inherit (inputs) nixpkgs clan-core;

  defaultModule = { config, lib, ... }: {
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
in
{
  flake = clan-core.lib.buildClan {
    clanName = "mic92";
    directory = self;
    specialArgs = {
      self = {
        inputs = self.inputs;
        nixosModules = self.nixosModules;
        packages = self.packages.x86_64-linux;
      };
      inputs = inputs;
    };
    pkgsForSystem = system: nixpkgs.legacyPackages.${system};

    machines = {
      bernie = {
        nixpkgs.pkgs = nixpkgs.legacyPackages.x86_64-linux;
        imports = [
          ./bernie/configuration.nix
          defaultModule
          inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x13
          inputs.home-manager.nixosModules.home-manager
          inputs.srvos.nixosModules.desktop
        ];
      };

      turingmachine = {
        nixpkgs.pkgs = nixpkgs.legacyPackages.x86_64-linux;
        imports = [
          ./turingmachine/configuration.nix
          defaultModule
          inputs.nixos-hardware.nixosModules.framework-13th-gen-intel
          inputs.nix-index-database.nixosModules.nix-index
          inputs.disko.nixosModules.disko
          { programs.nix-index-database.comma.enable = true; }
          #{
          #  imports = [
          #    ./turingmachine/modules/microvm.nix
          #    # for declarative MicroVM management
          #    self.inputs.microvm.nixosModules.host
          #  ];
          #}
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
        nixpkgs.pkgs = nixpkgs.legacyPackages.x86_64-linux;
        imports = [
          ./eve/configuration.nix
          defaultModule
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
        nixpkgs.pkgs = nixpkgs.legacyPackages.x86_64-linux;
        imports = [
          ./eva/configuration.nix
          defaultModule
          inputs.srvos.nixosModules.server
          inputs.srvos.nixosModules.mixins-nginx
          inputs.srvos.nixosModules.mixins-systemd-boot
          inputs.srvos.nixosModules.roles-prometheus
          inputs.disko.nixosModules.disko
        ];
      };

      blob64 = {
        nixpkgs.pkgs = nixpkgs.legacyPackages.aarch64-linux;
        imports = [
          ./blob64/configuration.nix
          defaultModule
          inputs.srvos.nixosModules.server
        ];
      };

      matchbox = {
        nixpkgs.pkgs = nixpkgs.legacyPackages.aarch64-linux;
        imports = [
          ./matchbox/configuration.nix
          defaultModule
          inputs.srvos.nixosModules.server
        ];
      };
    };
  };
}
