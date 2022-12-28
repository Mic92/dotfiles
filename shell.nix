{ inputs, ... }:
{

  imports = [
    inputs.treefmt-nix.flakeModule
  ];

  perSystem =
    { inputs'
    , pkgs
    , config
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
                # First deadnix
                ${pkgs.lib.getExe pkgs.deadnix} --edit "$@"
                # Then nixpkgs-fmt
                ${pkgs.lib.getExe pkgs.nixpkgs-fmt} "$@"
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
                ${pkgs.lib.getExe pkgs.shellcheck} --external-sources --source-path=SCRIPTDIR "$@"
                # Then format
                ${pkgs.lib.getExe pkgs.shfmt} -i 2 -s -w "$@"
              ''
              "--"
            ];
            includes = [ "*.sh" ];
            excludes = [ "zsh/*" ];
          };

          python = {
            command = "sh";
            options = [
              "-eucx"
              ''
                ${pkgs.lib.getExe pkgs.ruff} --fix "$@"
                ${pkgs.lib.getExe pkgs.python3.pkgs.black} "$@"
              ''
              "--" # this argument is ignored by bash
            ];
            includes = [ "*.py" ];
            excludes = [ "gdb/*" ];
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
          pkgs.python3.pkgs.invoke
          pkgs.python3.pkgs.deploykit

          config.treefmt.build.wrapper
        ];
      };
    };
}
