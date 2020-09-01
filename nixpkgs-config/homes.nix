{ self, nixpkgs, home-manager, nur }:

let
  hmConfiguration = { extraModules ? [] }:
    (home-manager.lib.homeManagerConfiguration {
      configuration = { ... }: {
        imports = extraModules ++ [
          ./common.nix
        ];
        nixpkgs.config.packageOverrides = pkgs: {
          nur = import "${nur}" {
            inherit pkgs;
            nurpkgs = pkgs;
          };
          bitwarden-wrapper = import ./bitwarden.nix {};
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
