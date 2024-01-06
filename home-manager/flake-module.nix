{ self, inputs, ... }: {
  imports = [ ./modules/neovim/flake-module.nix ];

  perSystem = { config, pkgs, lib, ... }:
    let
      homeManagerConfiguration = { extraModules ? [ ] }:
        (inputs.home-manager.lib.homeManagerConfiguration {
          modules = [
            {
              _module.args.self = self;
              _module.args.inputs = self.inputs;
              imports =
                extraModules
                ++ [
                  ./common.nix
                  inputs.nix-index-database.hmModules.nix-index
                  { programs.nix-index-database.comma.enable = true; }
                ];
            }
          ];
          inherit pkgs;
        });
    in
    {
      # selects the right home-manager configuration based on the hostname
      # Called from my .zshrc like this:
      # hm(){ nix run "$HOME/.homesick/repos/dotfiles#hm" -- "$@"; }
      apps.hm = {
        type = "app";
        program = "${pkgs.writeShellScriptBin "hm" ''
        set -x
        export PATH=${pkgs.lib.makeBinPath [pkgs.git pkgs.coreutils pkgs.findutils pkgs.nix pkgs.jq pkgs.unixtools.hostname]}
        declare -A profiles=(
          ["turingmachine"]="desktop"
          ["bernie"]="bernie"
          ["web01"]="mic92"
          ["bld2"]="mic92"
          ["eve"]="eve"
          ["mac01.numtide.com"]="mac-hetzner"
        )
        profile="common"
        user=$(id -un)
        host=$(hostname)
        if [[ -n ''${profiles["$host-$user"]} ]]; then
          profile=''${profiles["$host-$user"]};
        elif [[ -n ''${profiles[$host]:-} ]]; then
          profile=''${profiles[$host]}
        fi
        if [[ "''${1:-}" == profile ]]; then
          echo $profile
          exit 0
        fi
        ${inputs.home-manager.packages.${pkgs.system}.home-manager}/bin/home-manager --flake "${self}#$profile" "$@"
      ''}/bin/hm";
      };

      apps.bootstrap-dotfiles = {
        type = "app";
        program = "${pkgs.writeShellScriptBin "bootstrap-dotfiles" ''
          set -x
          export PATH=${pkgs.lib.makeBinPath [pkgs.git pkgs.coreutils pkgs.findutils pkgs.nix pkgs.jq pkgs.bash]}
          if [ ! -d "$HOME/.homesick/repos/homeshick" ]; then
            git clone --depth=1 https://github.com/andsens/homeshick.git "$HOME/.homesick/repos/homeshick"
          fi
          if [ ! -d "$HOME/.homesick/repos/dotfiles" ]; then
            "$HOME/.homesick/repos/homeshick/bin/homeshick" clone https://github.com/Mic92/dotfiles.git
          fi
          "$HOME/.homesick/repos/homeshick/bin/homeshick" symlink
          nix run ${self}#hm -- switch
        ''}/bin/bootstrap-dotfiles";
      };
      apps.default = config.apps.bootstrap-dotfiles;

      legacyPackages = {
        homeConfigurations = {
          # this one should work for aarch64-linux/x86_64-linux and macos
          common = homeManagerConfiguration { };

          mac-hetzner = homeManagerConfiguration {
            extraModules = [{ home.username = "hetzner"; }];
          };

        } // lib.optionalAttrs (pkgs.hostPlatform.system == "x86_64-linux") {
          desktop = homeManagerConfiguration {
            extraModules = [ ./desktop.nix ];
          };

          # different username
          mic92 = homeManagerConfiguration {
            extraModules = [{ home.username = "mic92"; }];
          };

          eve = homeManagerConfiguration {
            extraModules = [ ./eve.nix ];
          };
          bernie = homeManagerConfiguration {
            extraModules = [ ./bernie.nix ];
          };
        };
      };
    };
}
