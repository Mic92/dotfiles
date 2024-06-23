{ pkgs, ... }: {
  homebrew.casks = [ "iterm2" ];
  fonts.packages = [
    (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; })
  ];
}
