{ pkgs, ... }:
{
  homebrew.casks = [ "alacritty" ];
  fonts.packages = [ (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; }) ];
}
