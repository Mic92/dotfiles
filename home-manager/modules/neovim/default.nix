{ config
, pkgs
, ...
}: {
  home.packages = with pkgs; [
    neovim
    rnix-lsp
    rust-analyzer
    vale
    shellcheck
    gopls
    stylua
    nodePackages.pyright
    (
      if pkgs.stdenv.isDarwin
      then pkgs.sumneko-lua-language-server-mac
      else pkgs.sumneko-lua-language-server
    )
  ];
  xdg.dataHome = "${config.home.homeDirectory}/.data";
  # fzf-native
  xdg.dataFile."nvim/site/pack/packer/start/telescope-fzf-native.nvim/build/libfzf.so".source = "${pkgs.vimPlugins.telescope-fzf-native-nvim}/share/vim-plugins/telescope-fzf-native-nvim/build/libfzf.so";
  # tree-sitter parsers
  xdg.configFile."nvim/init.lua".source = ./init.lua;
  xdg.configFile."nvim/parser/c.so".source = "${pkgs.tree-sitter.builtGrammars.tree-sitter-c}/parser";
  xdg.configFile."nvim/parser/lua.so".source = "${pkgs.tree-sitter.builtGrammars.tree-sitter-lua}/parser";
  xdg.configFile."nvim/parser/rust.so".source = "${pkgs.tree-sitter.builtGrammars.tree-sitter-rust}/parser";
  xdg.configFile."nvim/parser/python.so".source = "${pkgs.tree-sitter.builtGrammars.tree-sitter-python}/parser";
}
