{ pkgs, lib, config, ... }:

{
  imports = [
    ./common.nix
  ];
  home.packages = [ (pkgs.callPackage ./weechat.nix {})];
}
