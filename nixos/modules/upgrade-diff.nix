{ pkgs, ... }:
{
  system.activationScripts.diff = ''
    ${pkgs.nix}/bin/nix store diff-closures /run/current-system "$systemConfig"
   '';
}
