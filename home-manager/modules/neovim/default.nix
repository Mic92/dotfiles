{ pkgs, self, ... }:
let
  inherit (self.packages.${pkgs.hostPlatform.system}) neovim treesitter-grammars;
  inherit (self.legacyPackages.${pkgs.hostPlatform.system}) nvim-lsp-packages;
in
{
  home.packages = nvim-lsp-packages ++ [ neovim ];
  # treesitter-grammars
  xdg.dataFile."nvim/site/parser".source = treesitter-grammars;
  xdg.dataFile."nvim/lib/libfzf.so".source = "${pkgs.vimPlugins.telescope-fzf-native-nvim}/build/libfzf.so";
}
