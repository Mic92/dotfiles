{ config
, pkgs
, lib
, inputs
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
  systemd.user.services.nvim-server = {
    Install.WantedBy = [ "default.target" ];
    Service = {
      Type = "simple";
      ExecStart = "${pkgs.neovim}/bin/nvim --headless --listen ~/.cache/nvim/server.pipe";
      Restart = "always";
      RestartSec = 1;
    };
  };
  home.packages = with pkgs; [
    (writeScriptBin "vimeditor" ''
      #!${pkgs.runtimeShell}
      exec ${pkgs.neovim}/bin/nvim --server ~/.cache/nvim/server.pipe --remote-ui --remote "$@"
    '')

    nodejs # copilot
    nil
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
  xdg.configFile."nvim".source = pkgs.runCommand "nvim" {} ''
    mkdir -p $out/parser

    ln -s ${inputs.astro-nvim}/* $out/
    rm $out/lua
    mkdir -p $out/lua
    ln -s ${inputs.astro-nvim}/lua/* $out/lua
    ln -s ${./user} $out/lua/user

    ${lib.concatMapStringsSep "\n" (name: ''
      ln -s ${pkgs.tree-sitter.builtGrammars."tree-sitter-${name}"}/parser $out/parser/${name}.so
    '') langs}
  '';
}
