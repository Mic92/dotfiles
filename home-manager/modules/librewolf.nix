{
  pkgs,
  lib,
  inputs,
  ...
}:

let
  firefoxExtensions = pkgs.callPackages ../../pkgs/firefox-extensions { };
  micsSkills = inputs.mics-skills.packages.${pkgs.stdenv.hostPlatform.system};
in
{
  # LibreWolf is installed via darwinModules/nix-casks.nix on macOS
  home.packages = lib.optionals pkgs.stdenv.isLinux [
    (pkgs.librewolf.override {
      extraPolicies = import ../../pkgs/librewolf-policies.nix {
        inherit (micsSkills) browser-cli-extension;
        inherit (firefoxExtensions) chrome-tab-gc-extension;
      };
    })
  ];
}
