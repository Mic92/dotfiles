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

    ocaml-ng.ocamlPackages_5_0.ocaml-lsp
    ocaml-ng.ocamlPackages_5_0.ocamlformat
    # does not build yet on aarch64
  ] ++ lib.optional (pkgs.stdenv.hostPlatform.system == "x86_64-linux") pkgs.deno
  ++ lib.optional (!pkgs.stdenv.hostPlatform.isDarwin) sumneko-lua-language-server;
in
stdenv.mkDerivation {
  name = "astro-nvim-config";
  phases = "installPhase";
  installPhase = ''
    mkdir -p $out/parser
    cp -r --reflink=auto ${inputs.astro-nvim}/* $out/

    pushd $out
    chmod -R +w .
    patch -p1 < ${./patches/0001-disable-neoconf.nvim.patch}
    popd

    ln -s ${./user} $out/lua/user

    ${lib.concatMapStringsSep "\n" (grammar: ''
      ln -s $(readlink -f ${grammar}/parser/*.so) $out/parser/${lib.last (builtins.split "-" grammar.name)}.so
    '') pkgs.vimPlugins.nvim-treesitter.withAllGrammars.dependencies}
  '';
  passthru.lspPackages = lspPackages;
}
