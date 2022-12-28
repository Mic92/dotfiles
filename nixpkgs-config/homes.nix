{ self, inputs, ... }: {
  perSystem = { pkgs, ... }: {
    apps.hm = {
      type = "app";
      program = "${pkgs.writeScriptBin "hm" ''
        export PATH=${pkgs.lib.makeBinPath [pkgs.git pkgs.coreutils pkgs.nix pkgs.jq]}
        declare -A profiles=(["turingmachine"]="desktop" ["eddie"]="desktop" ["eve"]="eve" ["bernie"]="bernie" ["grandalf"]="common-aarch64" ["yasmin"]="common-aarch64")
        profile=common
        if [[ -n ''${profiles[$HOSTNAME]:-} ]]; then
          profile=''${profiles[$HOSTNAME]}
        fi
        ${inputs.home-manager.packages.${pkgs.system}.home-manager}/bin/home-manager --flake "${self}#$profile" "$@"
      ''}/bin/hm";
    };
  };

  flake =
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
      homeConfigurations = {
        common = homeManagerConfiguration { };
        common-aarch64 = homeManagerConfiguration {
          system = "aarch64-linux";
        };

        desktop = homeManagerConfiguration {
          extraModules = [
            ./desktop.nix
            inputs.nix-index-database.hmModules.nix-index
            inputs.hyprland.homeManagerModules.default
            ({ pkgs, ... }: {
              programs.waybar.package = inputs.hyprland.packages.${pkgs.system}.waybar-hyprland;
              home.packages = [
                inputs.hyprland-contrib.packages.${pkgs.system}.grimblast
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
}
