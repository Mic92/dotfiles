{ lib, ... }:
{
  perSystem =
    {
      pkgs,
      config,
      ...
    }:
    {
      legacyPackages = {
        nvim-lsp-packages = with pkgs; [
          nodejs # copilot
          # tree-sitter-cli needs a C compiler to build parsers from source
          (pkgs.symlinkJoin {
            name = "tree-sitter-wrapped";
            paths = [ pkgs.tree-sitter ];
            nativeBuildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/tree-sitter \
                --prefix PATH : ${lib.makeBinPath [ pkgs.stdenv.cc ]}
            '';
          })

          # based on ./suggested-pkgs.json
          basedpyright
          bash-language-server
          clang-tools
          golangci-lint
          gopls
          lua-language-server
          marksman
          nil
          nixd
          nixfmt-rfc-style
          prettierd
          ruff
          selene
          shellcheck
          shfmt
          stylua
          vtsls
          yaml-language-server
          vscode-langservers-extracted
          # based on https://github.com/ray-x/go.nvim#go-binaries-install-and-update
          go
          delve
          ginkgo
          gofumpt
          golines
          gomodifytags
          gotests
          gotestsum
          gotools
          govulncheck
          iferr
          impl

          # others
          rust-analyzer
          clippy
          rustfmt

          zls
          terraform-ls
          taplo
          typos
          typos-lsp
        ];
      };
      packages = {
        neovim = pkgs.wrapNeovimUnstable pkgs.neovim-unwrapped (
          pkgs.neovimUtils.makeNeovimConfig {
            wrapRc = false;
            withRuby = false;
          }
        );
        nvim-open = pkgs.python3.pkgs.callPackage ./nvim-open.nix { };

        nvim = pkgs.callPackage ./nvim-standalone.nix {
          nvim-appname = "nvim-mic92";
          inherit (config.legacyPackages) nvim-lsp-packages;
        };
      };
    };
}
