{ config, lib, pkgs, ... }:

{
  imports = [
    ../../modules/telegraf.nix
  ];

  mic92.telegraf.mode = "push";
}
