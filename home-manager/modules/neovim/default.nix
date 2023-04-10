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
    "elisp"
    #"fluent"
    "go"
    "hcl"
    "haskell"
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
  xdg.dataFile."nvim/lazy/telescope-fzf-native.nvim/build/libfzf.so".source = "${pkgs.vimPlugins.telescope-fzf-native-nvim}/build/libfzf.so";
  # tree-sitter parsers
  #xdg.configFile."nvim/init.lua".source = ./init.lua;

  xdg.configFile = lib.mapAttrs (name: _: {
    source = "${pkgs.tree-sitter.builtGrammars."tree-sitter-${name}"}/parser";
    target = "nvim/parser/${name}.so";
  }) (lib.genAttrs langs (lang: lang));
}
