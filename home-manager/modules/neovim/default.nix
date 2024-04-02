{ pkgs, self, ... }:
let
  inherit (self.packages.${pkgs.hostPlatform.system}) nvim nvim-open treesitter-grammars;

  lspPackages =
    with pkgs;
    [
      nodejs # copilot
      terraform-ls
      nodePackages.pyright

      # based on ./suggested-pkgs.json
      gopls
      golangci-lint
      nodePackages.bash-language-server
      taplo-lsp
      marksman
      selene
      rust-analyzer
      yaml-language-server
      nil
      shellcheck
      shfmt
      ruff
      ruff-lsp
      nixfmt-rfc-style
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

      #ocaml-ng.ocamlPackages_5_0.ocaml-lsp
      #ocaml-ng.ocamlPackages_5_0.ocamlformat
      # does not build yet on aarch64
    ]
    ++ lib.optional (pkgs.stdenv.hostPlatform.system == "x86_64-linux") pkgs.deno
    ++ lib.optional (!pkgs.stdenv.hostPlatform.isDarwin) sumneko-lua-language-server;
in
{
  home.packages = lspPackages ++ [
    nvim
    nvim-open
  ];
  # treesitter-grammars
  xdg.dataFile."nvim/site/parser".source = treesitter-grammars;
}
