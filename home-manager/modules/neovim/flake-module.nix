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
          yaml-language-server
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
          zls
          terraform-ls
          deno
          taplo-lsp
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
            extraLuaPackages = ps: [ ps.tiktoken_core ];
          }
        );
        nvim-open = pkgs.python3.pkgs.callPackage ./nvim-open.nix { };
        nvim-install-treesitter =
          let
            grammars = lib.filterAttrs (
              n: _: lib.hasPrefix "tree-sitter-" n
            ) pkgs.vimPlugins.nvim-treesitter.builtGrammars;
          in
          (pkgs.writeShellScriptBin "nvim-install-treesitter" ''
            rm parser/*.so
            ${lib.concatMapStringsSep "\n" (name: ''
              ln -s ${grammars.${name}}/parser parser/${lib.removePrefix "tree-sitter-" name}.so
            '') (builtins.attrNames grammars)}
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
