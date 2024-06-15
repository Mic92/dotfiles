{ lib, ... }:
{
  perSystem =
    { pkgs, config, ... }:
    {
      legacyPackages = {
        nvim-lsp-packages =
          with pkgs;
          [
            nodejs # copilot
            terraform-ls
            pyright

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
      };
      packages = {
        neovim = pkgs.wrapNeovimUnstable pkgs.neovim-unwrapped (
          pkgs.neovimUtils.makeNeovimConfig {
            wrapRc = false;
            extraLuaPackages = ps: [ (ps.callPackage ./lua-tiktoken.nix { }) ];
          }
        );
        treesitter-grammars = let
          grammars = lib.filterAttrs (n: _: lib.hasPrefix "tree-sitter-" n) pkgs.vimPlugins.nvim-treesitter.builtGrammars;
          symlinks = lib.mapAttrsToList
            (name: grammar: "ln -s ${grammar}/parser $out/${lib.removePrefix "tree-sitter-" name}.so")
            grammars;
        in (pkgs.runCommand "treesitter-grammars" { } ''
          mkdir -p $out
          ${lib.concatStringsSep "\n" symlinks}
        '').overrideAttrs (_: {
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
