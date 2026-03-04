{ lib, ... }:
{
  imports = [
    ./common.nix
    ./modules/ai.nix
  ];
  home.username = "argocd";
  home.homeDirectory = lib.mkForce "/root";
}
