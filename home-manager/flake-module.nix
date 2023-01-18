{ self, inputs, ... }: {
  perSystem = { pkgs, ... }:
    let
      homeManagerConfiguration =
        { extraModules ? [ ]
        , system ? "x86_64-linux"
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
                ];
              home.username = "joerg";
              home.homeDirectory = "/home/joerg";
            }
          ];
          pkgs = inputs.nixpkgs.legacyPackages.${system};
        });
    in
    {
      apps.hm = {
        type = "app";
        program = "${pkgs.writeShellScriptBin "hm" ''
        set -x
        export PATH=${pkgs.lib.makeBinPath [pkgs.git pkgs.coreutils pkgs.nix pkgs.jq pkgs.unixtools.hostname]}
        declare -A profiles=(["turingmachine"]="desktop" ["eddie"]="desktop" ["eve"]="eve" ["bernie"]="bernie")
        profile="common-$(uname -s)"
        hostname
        if [[ -n ''${profiles[$(hostname)]:-} ]]; then
          profile=''${profiles[$(hostname)]}
        fi
        ${inputs.home-manager.packages.${pkgs.system}.home-manager}/bin/home-manager --flake "${self}#$profile" "$@"
      ''}/bin/hm";
      };
      legacyPackages = {
        homeConfigurations = {
          common = homeManagerConfiguration { };

          desktop = homeManagerConfiguration {
            extraModules = [
              ./desktop.nix
              inputs.nix-index-database.hmModules.nix-index
              inputs.hyprland.homeManagerModules.default
              ({ pkgs, ... }: {
                programs.waybar.package = inputs.hyprland.packages.${pkgs.system}.waybar-hyprland;
                wayland.windowManager.hyprland.enable = true;
                home.packages = [
                  inputs.hyprland-contrib.packages.${pkgs.system}.grimblast

                  inputs.nurl.packages.${pkgs.system}.default
                ];
              })
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
