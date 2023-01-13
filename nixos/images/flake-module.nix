{ self, lib, ... }:
let
  inherit (self.inputs) nixos-generators nur;
  defaultModule = { ... }: {
    imports = [
      ./base-config.nix
      self.inputs.nur.nixosModules.nur
    ];
    _module.args.inputs = self.inputs;
  };
in
{
  perSystem =
    { pkgs
    , self'
    , ...
    }:
    {
      packages = {
        matchbox-image = self.nixosConfigurations.matchbox.config.system.build.sdImage;

        install-iso = nixos-generators.nixosGenerate {
          inherit pkgs;
          modules = [
            defaultModule
          ];
          format = "install-iso";
        };

        netboot = pkgs.callPackage ./netboot.nix {
          inherit pkgs;
          inherit (lib) nixosSystem;
          extraModules = [
            defaultModule
          ];
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
  # for debugging
  #flake.nixosConfigurations = {
  #  sd-image = lib.nixosSystem {
  #    modules = [
  #      {
  #        nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  #      }
  #      defaultModule
  #    ];
  #  };
  #};
}
