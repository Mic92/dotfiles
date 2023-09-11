{ inputs
, tree-sitter
, lib
, pkgs
, stdenv
}:
let
  langs = [
    # "agda"
    #Error executing vim.schedule lua callback: UnhandledPromiseRejection with the reason:                                      
    #...ed-0.9.1/share/nvim/runtime/lua/vim/treesitter/query.lua:259: query: invalid node type at position 195 for language bash
    #"bash"
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
    "toml"
  ];
  lspPackages = with pkgs; [
    nodejs # copilot
    vale
    terraform-ls
    nodePackages.pyright
    sumneko-lua-language-server

    # based on ./suggested-pkgs.json
    gopls
    golangci-lint
    nodePackages.bash-language-server
    taplo-lsp
    marksman
    rust-analyzer
    yaml-language-server
    nil
    shellcheck
    shfmt
    isort
    black
    ruff
    nixpkgs-fmt
    terraform-ls
    clang-tools
    nodePackages.prettier
    stylua
    # based on https://github.com/ray-x/go.nvim#go-binaries-install-and-update
    go
    gofumpt
    gomodifytags
    gotools
    delve
    golines
    gomodifytags
    gotests
    iferr
    impl
    reftools
    ginkgo
    richgo
    govulncheck

    # does not build yet on aarch64
  ] ++ lib.optional (pkgs.stdenv.hostPlatform.system == "x86_64-linux") pkgs.deno;
in
stdenv.mkDerivation {
  name = "astro-nvim-config";
  phases = "installPhase";
  installPhase = ''
    mkdir -p $out/parser

    ln -s ${inputs.astro-nvim}/* $out/
    rm $out/lua
    mkdir -p $out/lua
    ln -s ${inputs.astro-nvim}/lua/* $out/lua
    ln -s ${./user} $out/lua/user

    ${lib.concatMapStringsSep "\n" (name: ''
      ln -s ${tree-sitter.builtGrammars."tree-sitter-${name}"}/parser $out/parser/${name}.so
    '') langs}
  '';
  passthru.lspPackages = lspPackages;
}
