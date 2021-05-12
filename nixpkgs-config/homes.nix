{ self, nixpkgs, home-manager, nur, nix-doom-emacs }:

let
  hmConfiguration = { extraModules ? [ ], system ? "x86_64-linux" }:
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
      inherit system;
      homeDirectory = "/home/joerg";
      username = "joerg";
    });
in
{
  common = hmConfiguration { };
  common-aarch64 = hmConfiguration {
    system = "aarch64-linux";
  };

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
