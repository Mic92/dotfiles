{ self
, nixpkgs
, home-manager
, nur
, doom-emacs
, emacs-overlay
, ...
}:

let
  hmConfiguration = { extraModules ? [ ]
                    , system ? "x86_64-linux"
                    }:
    (home-manager.lib.homeManagerConfiguration {
      configuration = { ... }: {
        imports = extraModules ++ [
          ./common.nix
        ];
        nixpkgs.config = import ./config.nix {
          pkgs = nixpkgs;
          nurFun = import nur;
        };
        nixpkgs.overlays = [
          emacs-overlay.overlay
          (self: super: {
            doomEmacsRevision = doom-emacs.rev;
          })
        ];
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
