{ lib, ... }:
{
  perSystem =
    {
      self',
      inputs',
      pkgs,
      config,
      ...
    }:
    {
      legacyPackages = {
        nvim-lsp-packages =
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
      };
      packages = {
        nvim-open = pkgs.python3Packages.callPackage ./nvim-open.nix { };
        neovim = pkgs.wrapNeovimUnstable pkgs.neovim-unwrapped (
          pkgs.neovimUtils.makeNeovimConfig {
            wrapRc = false;
            extraLuaPackages = ps: [ (ps.callPackage ./lua-tiktoken.nix { }) ];
          }
        );
        treesitter-grammars = pkgs.runCommand "treesitter-grammars" { } (
          lib.concatMapStringsSep "\n" (grammar: ''
            mkdir -p $out
            ln -s $(readlink -f ${grammar}/parser/*.so) $out/${lib.last (builtins.split "-" grammar.name)}.so
          '') pkgs.vimPlugins.nvim-treesitter.withAllGrammars.dependencies
        );

        nvim = pkgs.callPackage ./nvim-standalone.nix {
          nvim-appname = "nvim-mic92";
          inherit (config.packages) treesitter-grammars;
          inherit (config.legacyPackages) nvim-lsp-packages;
        };
      };
    };
}
