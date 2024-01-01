{ inputs, lib, ... }:
{

  imports = [
    inputs.treefmt-nix.flakeModule
  ];

  perSystem =
    { inputs'
    , pkgs
    , ...
    }: {
      treefmt = {
        # Used to find the project root
        projectRootFile = ".git/config";

        programs.hclfmt.enable = true;
        programs.mypy.enable = true;
        programs.mypy.directories = {
          "tasks" = {
            directory = ".";
            modules = [ ];
            files = [ "**/tasks.py" ];
            extraPythonPackages = [
              pkgs.python3.pkgs.deploykit
              pkgs.python3.pkgs.invoke
            ];
          };
          "nixos/eva/modules/prometheus" = { };
          "openwrt" = { };
          "home-manager/modules/neovim" = {
            options = [ "--ignore-missing-imports" ];
          };
        };

        settings.formatter = {
          nix = {
            command = "sh";
            options = [
              "-eucx"
              ''
                ${pkgs.deadnix}/bin/deadnix --edit "$@"
                ${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt "$@"
              ''
              "--"
            ];
            includes = [ "*.nix" ];
            excludes = [ "nix/sources.nix" ];
          };
          shell = {
            command = "sh";
            options = [
              "-eucx"
              ''
                # First shellcheck
                ${pkgs.shellcheck}/bin/shellcheck --external-sources --source-path=SCRIPTDIR "$@"
                # Then format
                ${pkgs.shfmt}/bin/shfmt -i 2 -s -w "$@"
              ''
              "--"
            ];
            includes = [ "*.sh" ];
            excludes = [
              "zsh/*"
              "gdb/*"
            ];
          };

          python = {
            command = "sh";
            options = [
              "-eucx"
              ''
                ${pkgs.ruff}/bin/ruff --fix "$@"
                ${pkgs.ruff}/bin/ruff format "$@"
              ''
              "--" # this argument is ignored by bash
            ];
            includes = [ "*.py" ];
            excludes = [
              "gdb/*"
              "zsh/*"
            ];
          };
        };
      };

      # Definitions like this are entirely equivalent to the ones
      # you may have directly in flake.nix.
      devShells.default = pkgs.mkShellNoCC {
        nativeBuildInputs = [
          inputs'.fast-flake-update.packages.default
          pkgs.python3.pkgs.invoke
          pkgs.python3.pkgs.deploykit
        ] ++ lib.optionals (!pkgs.stdenv.isDarwin) [
          inputs'.clan-core.packages.default
          pkgs.bubblewrap
        ];
      };
    };
}
