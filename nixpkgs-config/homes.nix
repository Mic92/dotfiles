{ self, nixpkgs, home-manager, nur }:

let
  hmConfiguration = { extraModules ? [] }:
    (home-manager.lib.homeManagerConfiguration {
      configuration = { ... }: {
        imports = extraModules ++ [
          ./common.nix
        ];
        nixpkgs.config = import ./config.nix {
          pkgs = nixpkgs;
          nur = import nur;
        };
      };
      system = "x86_64-linux";
      homeDirectory = "/home/joerg";
      username = "joerg";
    });
in {
  common = hmConfiguration {};
  desktop = hmConfiguration {
    extraModules = [ ./desktop.nix ];
  };
  eve = hmConfiguration {
    extraModules = [ ./eve.nix ];
  };
}
