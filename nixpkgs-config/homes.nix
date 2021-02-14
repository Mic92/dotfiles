{ self, nixpkgs, home-manager, nur, nix-doom-emacs }:

let
  hmConfiguration = { extraModules ? [] }:
    (home-manager.lib.homeManagerConfiguration {
      configuration = { ... }: {
        imports = extraModules ++ [
          nix-doom-emacs.hmModule
          ./common.nix
        ];
        nixpkgs.config = import ./config.nix {
          pkgs = nixpkgs;
          nurFun = import nur;
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
  bernie = hmConfiguration {
    extraModules = [ ./bernie.nix ];
  };
}
