{
  self,
  inputs,
  lib,
  ...
}:
let
  inherit (self.inputs) nixos-generators;
  defaultModule =
    { ... }:
    {
      imports = [ ./base-config.nix ];
      _module.args.inputs = self.inputs;
    };
in
{
  perSystem =
    { pkgs, self', ... }:
    {
      packages = lib.optionalAttrs pkgs.stdenv.isLinux {
        #install-iso = nixos-generators.nixosGenerate {
        #  inherit pkgs;
        #  modules = [ defaultModule ];
        #  format = "install-iso";
        #};

        netboot = pkgs.callPackage ./netboot.nix {
          inherit pkgs;
          inherit (lib) nixosSystem;
          extraModules = [ defaultModule ];
        };

        netboot-pixie-core = pkgs.callPackage ./netboot-pixie-core.nix {
          inherit (self'.packages) netboot;
        };

        nspawn-template = import ./nspawn-template.nix {
          inherit nixos-generators;
          inherit pkgs;
        };
      };
    };
  clan.machines = {
    installer = (
      { modulesPath, ... }:
      {
        imports = [
          defaultModule
          inputs.clan-core.clanModules.diskLayouts

          "${toString modulesPath}/profiles/installation-device.nix"
          "${toString modulesPath}/profiles/base.nix"
          "${toString modulesPath}/profiles/all-hardware.nix"
        ];
        clan.diskLayouts.singleDiskExt4.device = "/dev/invalid";
        nixpkgs.pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
      }
    );
  };

  flake.nixosConfigurations.your-machine = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      {
        # TODO: add your NixOS configuration here
        boot.loader.systemd-boot.enable = true;
        imports = [ self.inputs.disko.nixosModules.disko ];
        disko.devices = {
          disk = {
            vdb = {
              device = "/dev/disk/by-id/some-disk-id";
              type = "disk";
              content = {
                type = "gpt";
                partitions = {
                  ESP = {
                    type = "EF00";
                    size = "500M";
                    content = {
                      type = "filesystem";
                      format = "vfat";
                      mountpoint = "/boot";
                    };
                  };
                  root = {
                    size = "100%";
                    content = {
                      type = "filesystem";
                      format = "ext4";
                      mountpoint = "/";
                    };
                  };
                };
              };
            };
          };
        };
      }
    ];
  };
}
