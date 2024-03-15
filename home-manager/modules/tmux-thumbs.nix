{ pkgs, ... }:
{
  home.packages = with pkgs; [ tmuxPlugins.tmux-thumbs ];
  home.extraOutputsToInstall = [ "share/tmux-plugins" ];
}
