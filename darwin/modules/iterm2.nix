{ pkgs, ... }:
{
  homebrew.casks = [ "iterm2" ];
  fonts.packages = [ pkgs.nerd-fonts.fira-code ];
}
