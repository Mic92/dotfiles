{
  pkgs,
  lib,
  self,
  self',
  system,
  ...
}:
let
  inputs = self.inputs;

  homeManagerConfiguration =
    {
      extraModules ? [ ],
    }:
    (inputs.home-manager.lib.homeManagerConfiguration {
      extraSpecialArgs = { inherit self inputs; };
      modules = extraModules ++ [
        ./common.nix
        inputs.nix-index-database.homeModules.nix-index
        { programs.nix-index-database.comma.enable = true; }
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
      export PATH=${
        pkgs.lib.makeBinPath [
          pkgs.gitMinimal
          pkgs.coreutils
          pkgs.findutils
          pkgs.jq
          pkgs.unixtools.hostname
          pkgs.nixVersions.latest
        ]
      }
      declare -A profiles=(
        ["turingmachine"]="desktop"
        ["jacquardmachine"]="desktop"
        ["bernie"]="bernie"
        ["web01"]="mic92"
        ["bld2"]="mic92"
        ["eve"]="eve"
        ["utm-vm"]="utm-vm"
        ["evo"]="macos"
      )
      profile="common"
      user=$(id -un)
      host=$(hostname)
      if [[ -n ''${profiles["$host-$user"]} ]]; then
        profile=''${profiles["$host-$user"]};
      elif [[ -n ''${profiles[$host]:-} ]]; then
        profile=''${profiles[$host]}
      elif [[ "$host" == coder-* ]]; then
        profile="coder"
      fi
      if [[ "''${1:-}" == profile ]]; then
        echo $profile
        exit 0
      fi
      ${
        inputs.home-manager.packages.${system}.home-manager
      }/bin/home-manager --option keep-going true --flake "${self}#$profile" "$@"
    ''}/bin/hm";
  };

  apps.bootstrap-dotfiles = {
    type = "app";
    program = "${pkgs.writeShellScriptBin "bootstrap-dotfiles" ''
      set -x
      export PATH=${
        pkgs.lib.makeBinPath [
          pkgs.gitMinimal
          pkgs.coreutils
          pkgs.findutils
          pkgs.nix
          pkgs.jq
          pkgs.bash
        ]
      }
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
  apps.default = self'.apps.bootstrap-dotfiles;

  legacyPackages = {
    homeConfigurations = {
      # this one should work for aarch64-linux/x86_64-linux and macos
      common = homeManagerConfiguration { };
      utm-vm = homeManagerConfiguration { extraModules = [ ./utm-vm.nix ]; };
    }
    // lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "aarch64-darwin") {
      macos = homeManagerConfiguration { extraModules = [ ./macos.nix ]; };
    }
    // lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
      desktop = homeManagerConfiguration { extraModules = [ ./desktop.nix ]; };

      # different username
      mic92 = homeManagerConfiguration { extraModules = [ { home.username = "mic92"; } ]; };

      eve = homeManagerConfiguration { extraModules = [ ./eve.nix ]; };
      bernie = homeManagerConfiguration { extraModules = [ ./bernie.nix ]; };
      coder = homeManagerConfiguration { extraModules = [ ./coder.nix ]; };
    };
  };
}
