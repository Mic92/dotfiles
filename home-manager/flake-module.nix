{ self, inputs, ... }: {
  perSystem = { config, pkgs, lib, ... }:
    let
      homeManagerConfiguration =
        { extraModules ? [ ]
        ,
        }: (inputs.home-manager.lib.homeManagerConfiguration {
          modules = [
            {
              _module.args.self = self;
              _module.args.inputs = self.inputs;
              imports =
                extraModules
                ++ [
                  ./common.nix
                  inputs.nur.hmModules.nur
                  inputs.nix-index-database.hmModules.nix-index
                  { programs.nix-index-database.comma.enable = true; }
                ];
              home.username = "joerg";
              home.homeDirectory = "/home/joerg";
            }
          ];
          inherit pkgs;
        });
    in
    {
      apps.hm = {
        type = "app";
        program = "${pkgs.writeShellScriptBin "hm" ''
        set -x
        export PATH=${pkgs.lib.makeBinPath [pkgs.git pkgs.coreutils pkgs.nix pkgs.jq pkgs.unixtools.hostname]}
        declare -A profiles=(["turingmachine"]="desktop" ["eddie"]="desktop" ["eve"]="eve" ["bernie"]="bernie")
        profile="common"
        if [[ -n ''${profiles[$(hostname)]:-} ]]; then
          profile=''${profiles[$(hostname)]}
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
          export PATH=${pkgs.lib.makeBinPath [pkgs.git pkgs.coreutils pkgs.nix pkgs.jq pkgs.bash]}
          if [ ! -d "$HOME/.homesick/repos/homeshick" ]; then
            git clone --depth=1 https://github.com/andsens/homeshick.git "$HOME/.homesick/repos/homeshick"
          fi
          if [ ! -d "$HOME/.homesick/repos/dotfiles" ]; then
            "$HOME/.homesick/repos/homeshick/bin/homeshick" clone https://github.com/Mic92/dotfiles.git
          fi
          nix run ${self}#hm -- switch
        ''}/bin/bootstrap-dotfiles";
      };
      apps.default = config.apps.bootstrap-dotfiles;

      legacyPackages = {
        homeConfigurations = {
          common = homeManagerConfiguration { };
        } // lib.optionalAttrs (pkgs.hostPlatform.system == "x86_64-linux") {
          desktop = homeManagerConfiguration {
            extraModules = [
              ./desktop.nix
            ];
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
