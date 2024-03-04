{ config
, pkgs
, self
, ...
}:
let
  inherit (self.packages.${pkgs.hostPlatform.system}) astro-nvim-config nvim-open;
in
{
  home.packages = [ astro-nvim-config.neovim nvim-open ] ++ astro-nvim-config.lspPackages;
  xdg.dataHome = "${config.home.homeDirectory}/.data";
  xdg.dataFile."nvim/lazy/telescope-fzf-native.nvim/build/libfzf.so".source = "${pkgs.vimPlugins.telescope-fzf-native-nvim}/build/libfzf.so";
  xdg.configFile."nvim".source = astro-nvim-config;
}
