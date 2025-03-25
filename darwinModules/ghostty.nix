{ pkgs, ... }:
{
  homebrew.casks = [ "ghostty" ];
  fonts.packages = [ pkgs.nerd-fonts.fira-code ];
}
