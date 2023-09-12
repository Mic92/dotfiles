{ inputs
, lib
, pkgs
, stdenv
}:
let
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

    ${lib.concatMapStringsSep "\n" (grammar: ''
      ln -s $(readlink -f ${grammar}/parser/*.so) $out/parser/${lib.last (builtins.split "-" grammar.name)}.so
    '') pkgs.vimPlugins.nvim-treesitter.withAllGrammars.dependencies}
  '';
  passthru.lspPackages = lspPackages;
}
