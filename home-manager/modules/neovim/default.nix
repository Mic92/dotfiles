{ config, pkgs, self, ... }:
let
  inherit (self.packages.${pkgs.hostPlatform.system}) neovim treesitter-grammars;
  inherit (self.legacyPackages.${pkgs.hostPlatform.system}) nvim-lsp-packages;
in
{
  home.packages = nvim-lsp-packages ++ [ neovim ];
  # treesitter-grammars
  xdg.dataFile."nvim/site/parser".source = treesitter-grammars;

  home.activation.nvim = ''
    echo "${treesitter-grammars.rev}" > "${config.xdg.configHome}/nvim/treesitter-rev"
  '';

  xdg.dataFile."nvim/lib/libfzf.so".source = "${pkgs.vimPlugins.telescope-fzf-native-nvim}/build/libfzf.so";
}
