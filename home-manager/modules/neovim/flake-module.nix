{ lib, ... }:
{
  perSystem =
    {
      self',
      pkgs,
      config,
      ...
    }:
    {
      legacyPackages = {
        nvim-lsp-packages = with pkgs; [
          nodejs # copilot
          tree-sitter # needed by astrocore treesitter module

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

          self'.packages.nvim-install-treesitter
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

        nvim-treesitter-plugins =
          let
            grammars = lib.filterAttrs (
              n: _: lib.hasPrefix "tree-sitter-" n
            ) pkgs.vimPlugins.nvim-treesitter.builtGrammars;
          in
          pkgs.runCommand "nvim-treesitter-plugins" { } ''
            mkdir -p $out/lib/nvim-treesitter-gammars
            ${lib.concatMapStringsSep "\n" (name: ''
              ln -s ${grammars.${name}}/parser $out/lib/nvim-treesitter-gammars/${lib.removePrefix "tree-sitter-" name}.so
            '') (builtins.attrNames grammars)}
          '';

        nvim-install-treesitter =
          (pkgs.writeShellScriptBin "nvim-install-treesitter" ''
            set -euo pipefail nullglob
            mkdir -p parser
            rm -rf parser/*.so

            # prefer home-manager version if it exists, because it doesn't get stale links.
            if [ -d $HOME/.nix-profile/lib/nvim-treesitter-plugins ]; then
              ln -s $HOME/.nix-profile/lib/nvim-treesitter-plugins/lib/nvim-treesitter-gammars/*.so parser
            else
              ln -s ${self'.packages.nvim-treesitter-plugins}/lib/nvim-treesitter-gammars/*.so parser
            fi
          '').overrideAttrs
            (_: {
              passthru.rev = pkgs.vimPlugins.nvim-treesitter.src.rev;
            });

        nvim = pkgs.callPackage ./nvim-standalone.nix {
          nvim-appname = "nvim-mic92";
          inherit (config.packages) nvim-install-treesitter;
          inherit (config.legacyPackages) nvim-lsp-packages;
        };
      };
    };
}
