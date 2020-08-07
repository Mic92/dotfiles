{ self, nixpkgs, home-manager, nur }:

let
  pkgs = nixpkgs.legacyPackages."x86_64-linux";
  extendedLib = import "${home-manager}/modules/lib/stdlib-extended.nix" pkgs.lib;
  buildHomeManager = configuration: rec {
    hmModules = import "${home-manager}/modules/modules.nix" {
      inherit pkgs;
      lib = extendedLib;
      useNixpkgsModule = true;
    };
    module = extendedLib.evalModules {
      modules = [{
        nixpkgs.config.packageOverrides = pkgs: {
          nur = import "${nur}" {
            inherit pkgs;
          };
        };
      } configuration
      ] ++ hmModules;
    };
    activate = pkgs.writeShellScript "home-manager-activate" ''
      #!${pkgs.runtimeShell}
      exec ${module.config.home.activationPackage}/activate
    '';
  };
in {
  common = buildHomeManager "${self}/nixpkgs-config/common.nix";
  desktop = buildHomeManager "${self}/nixpkgs-config/desktop.nix";
}
