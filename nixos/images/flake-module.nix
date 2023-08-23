{ self, lib, ... }:
let
  inherit (self.inputs) nixos-generators;
  defaultModule = { ... }: {
    imports = [
      ./base-config.nix
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
      packages = lib.optionalAttrs pkgs.stdenv.isLinux {
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
