{ lib, ... }:
{
  perSystem =
    { pkgs, config, ... }:
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
        treesitter-grammars =
          let
            grammars = lib.filterAttrs (
              n: _: lib.hasPrefix "tree-sitter-" n
            ) pkgs.vimPlugins.nvim-treesitter.builtGrammars;
            symlinks = lib.mapAttrsToList (
              name: grammar: "ln -s ${grammar}/parser $out/${lib.removePrefix "tree-sitter-" name}.so"
            ) grammars;
          in
          (pkgs.runCommand "treesitter-grammars" { } ''
            mkdir -p $out
            ${lib.concatStringsSep "\n" symlinks}
          '').overrideAttrs
            (_: {
              passthru.rev = pkgs.vimPlugins.nvim-treesitter.src.rev;
            });

        nvim = pkgs.callPackage ./nvim-standalone.nix {
          nvim-appname = "nvim-mic92";
          inherit (config.packages) treesitter-grammars;
          inherit (config.legacyPackages) nvim-lsp-packages;
        };
      };
    };
}
