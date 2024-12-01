{ pkgs, ... }:
{
  homebrew.casks = [ "alacritty" ];
  fonts.packages = [ pkgs.nerd-fonts.fira-code ];
}
