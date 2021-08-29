{ pkgs, ... }:
{
  system.activationScripts.diff = ''
    ${pkgs.nixUnstable}/bin/nix store diff-closures /run/current-system "$systemConfig"
   '';
}
