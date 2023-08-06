{ inputs, ... }:
{

  imports = [
    inputs.treefmt-nix.flakeModule
  ];

  perSystem =
    { inputs'
    , pkgs
    , config
    , lib
    , ...
    }: {
      treefmt = {
        # Used to find the project root
        projectRootFile = "flake.lock";

        programs.terraform.enable = true;

        settings.formatter = {
          nix = {
            command = "sh";
            options = [
              "-eucx"
              ''
                export PATH=${lib.makeBinPath [ pkgs.coreutils pkgs.findutils pkgs.deadnix pkgs.nixpkgs-fmt ]}
                deadnix --edit "$@"
                nixpkgs-fmt "$@"
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
          inputs'.sops-nix.packages.sops-import-keys-hook
          inputs'.fast-flake-update.packages.default
          pkgs.python3.pkgs.invoke
          pkgs.python3.pkgs.deploykit

          config.treefmt.build.wrapper
        ];
      };
    };
}
