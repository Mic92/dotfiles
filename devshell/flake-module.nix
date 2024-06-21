{ inputs, lib, ... }:
{

  imports = [ inputs.treefmt-nix.flakeModule ];

  perSystem =
    { inputs', pkgs, ... }:
    {
      treefmt =
        { ... }:
        {
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
          programs.deadnix.enable = true;
          programs.nixfmt.enable = true;
          programs.nixfmt.package = pkgs.nixfmt-rfc-style;
          programs.shellcheck.enable = true;

          settings.formatter.shellcheck.options = [
            "--external-sources"
            "--source-path=SCRIPTDIR"
          ];

          programs.shfmt.enable = true;

          programs.ruff.format = true;
          programs.ruff.check = true;

          settings.formatter.ruff-check.excludes = [
            "gdb/*"
            "zsh/*"
            "home/.config/qtile/*"
            "home/.emacs/*"
          ];
          settings.formatter.ruff-format.excludes = [
            "gdb/*"
            "zsh/*"
          ];
          settings.formatter.shfmt.excludes = [
            "gdb/*"
            "zsh/*"
          ];
          settings.formatter.shellcheck.excludes = [
            "gdb/*"
            "zsh/*"
          ];
        };

      # Definitions like this are entirely equivalent to the ones
      # you may have directly in flake.nix.
      devShells.default = pkgs.mkShellNoCC {
        nativeBuildInputs = [
          inputs'.fast-flake-update.packages.default
          pkgs.python3.pkgs.invoke
          pkgs.python3.pkgs.deploykit
          inputs'.clan-core.packages.default
        ] ++ lib.optionals (!pkgs.stdenv.isDarwin) [ pkgs.bubblewrap ];
      };
    };
}
