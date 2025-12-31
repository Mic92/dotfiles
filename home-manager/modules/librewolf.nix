{
  pkgs,
  lib,
  ...
}:

let
  firefoxExtensions = pkgs.callPackages ../../pkgs/firefox-extensions { };
in
{
  # LibreWolf is installed via darwinModules/nix-casks.nix on macOS
  home.packages = lib.optionals pkgs.stdenv.isLinux [
    (pkgs.librewolf.override {
      extraPolicies = import ../../pkgs/librewolf-policies.nix { inherit firefoxExtensions; };
    })
  ];
}
