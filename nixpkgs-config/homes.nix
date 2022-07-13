{self, ...}: {
  perSystem = {
    pkgs,
    ...
  }: {
    apps.hm = {
      type = "app";
      program = "${pkgs.writeScriptBin "hm" ''
        declare -A profiles=(["turingmachine"]="desktop" ["eddie"]="desktop" ["eve"]="eve" ["bernie"]="bernie" ["grandalf"]="common-aarch64" ["yasmin"]="common-aarch64")
        profile=common
        if [[ -n ''${profiles[$HOSTNAME]:-} ]]; then
          profile=''${profiles[$HOSTNAME]}
        fi
        usage() {
          echo "hm (profile|build|switch)"
          exit 0
        }
        main() {
          cmd=$1
          shift
          case "$cmd" in
          -h|--help)
            usage
            ;;
          profile)
            echo "$profile"
            ;;
          build)
            nix build --no-link --show-trace --json "${toString ./..}#hmConfigurations.''${profile}.activationPackage" "$@" | jq -r '.[] | .outputs | .out'
            ;;
          switch)
            oldpath=$(realpath /nix/var/nix/profiles/per-user/$USER/home-manager)
            path=$(main build "$@")
            if [[ -e $oldpath ]]; then
              nix store diff-closures "$oldpath" "$path"
            fi
            $path/activate
            ;;
          esac
        }
        if [[ "$#" -lt 1 ]]; then
          usage
        fi
        main "$@"
      ''}/bin/hm";
    };
  };

  flake = let
    inherit (self.inputs) nixpkgs home-manager nur doom-emacs emacs-overlay;
    hmConfiguration = {
      extraModules ? [],
      system ? "x86_64-linux",
    }: (home-manager.lib.homeManagerConfiguration {
      modules = [
        {
          imports =
            extraModules
            ++ [
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
          home.username = "joerg";
          home.homeDirectory = "/home/joerg";
        }
      ];
      pkgs = nixpkgs.legacyPackages.${system};
    });
  in {
    hmConfigurations = {
      common = hmConfiguration {};
      common-aarch64 = hmConfiguration {
        system = "aarch64-linux";
      };

      desktop = hmConfiguration {
        extraModules = [./desktop.nix];
      };
      eve = hmConfiguration {
        extraModules = [./eve.nix];
      };
      bernie = hmConfiguration {
        extraModules = [./bernie.nix];
      };
    };
  };
}
