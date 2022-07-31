{ pkgs, ... }: {
  programs.zsh.enable = true;
  programs.zsh.enableCompletion = true;
  # do this in private dotfiles
  programs.zsh.enableGlobalCompInit = false;
  programs.zsh.interactiveShellInit = ''
    source ${pkgs.zsh-nix-shell}/share/zsh-nix-shell/nix-shell.plugin.zsh
  '';
}
