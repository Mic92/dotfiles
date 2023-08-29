{ inputs, ... }:
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

        programs.terraform.enable = true;
        programs.mypy.enable = true;
        programs.mypy.directories = {
          "tasks" = {
            directory = ".";
            modules = [ ];
            files = [ "**/tasks.py" ];
            extraPythonPackages = [
              pkgs.python3.pkgs.deploykit
              (pkgs.python3.pkgs.buildPythonPackage rec {
                pname = "types-invoke";
                version = "2.0.0.9";
                src = pkgs.python3.pkgs.fetchPypi {
                  inherit pname version;
                  hash = "sha256-5l+xnf3ZIztpwhoPK7mnm1FwWggVfc7O2TFnKGQsbds=";
                };
              })
            ];
          };
          "nixos/eve/modules/buildbot" = { };
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
                ${pkgs.python3.pkgs.black}/bin/black "$@"
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
        sopsPGPKeyDirs = [ "./nixos/secrets/keys" ];
        sopsCreateGPGHome = true;
        nativeBuildInputs = [
          inputs'.clan-core.packages.default
          inputs'.sops-nix.packages.sops-import-keys-hook
          inputs'.fast-flake-update.packages.default
          pkgs.python3.pkgs.invoke
          pkgs.python3.pkgs.deploykit
        ];
      };
    };
}
