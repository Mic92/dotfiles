{ config, lib, pkgs, ... }:

{
  imports = [
    ../../../modules/telegraf.nix
    ./private.nix
    ./uni.nix
    ./krebs.nix
    ./nix-community.nix
  ];
}
