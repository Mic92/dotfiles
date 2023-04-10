{ config
, pkgs
, lib
, ...
}: let
  langs = [
    # "agda"
    "bash"
    "c"
    "c-sharp"
    "cpp"
    "css"
    "elm"
    #"fluent"
    "go"
    "hcl"
    "html"
    "janet-simple"
    "java"
    "javascript"
    "jsdoc"
    "json"
    "julia"
    "ocaml"
    "pgn"
    "php"
    "python"
    "ruby"
    "rust"
    "scala"
    # "swift"
    "typescript"
    "yaml"
    "nix"
    "lua"
    "markdown-inline"
    "perl"
    "make"
  ];

  grammars = lib.getAttrs (map (lang: "tree-sitter-${lang}") langs) pkgs.tree-sitter.builtGrammars;

in {
  home.packages = with pkgs; [
    neovim
    nodejs
    rnix-lsp
    rust-analyzer
    vale
    shellcheck
    gopls
    stylua
    terraform-ls
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
  #xdg.configFile."nvim/init.lua".source = ./init.lua;

  xdg.configFile = lib.mapAttrs (name: value: {
    source = "${value}/parser";
    target = "nvim/parser/${lib.removePrefix "tree-sitter-" name}.so";
  }) grammars;
}
