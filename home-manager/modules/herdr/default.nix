{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.programs.herdr;

  # herdr's plugin registry (~/.config/herdr/plugins.json) is normally written
  # imperatively by `herdr plugin install/link`. We generate it from Nix-built
  # plugin packages instead so the plugin set is reproducible. herdr re-reads
  # each herdr-plugin.toml on startup, so only id/name/version/paths are needed.
  registry = pkgs.runCommand "herdr-plugins.json" { nativeBuildInputs = [ pkgs.python3 ]; } ''
    python3 ${./registry.py} ${lib.escapeShellArgs (map toString cfg.plugins)} > $out
  '';
in
{
  options.programs.herdr = {
    enable = lib.mkEnableOption "herdr, terminal workspace manager for AI agents";

    package = lib.mkOption {
      type = lib.types.package;
      description = "herdr package to install.";
    };

    plugins = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = ''
        herdr plugins to register declaratively. Each package must contain a
        herdr-plugin.toml at its root; build steps belong in the derivation
        (herdr's manifest [[build]] commands are not run for linked plugins).
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    # plugins.json must stay a real writable file: herdr rewrites it under a
    # lock on enable/disable and would otherwise clobber a store symlink and
    # break the next home-manager switch. Copying resets any imperative
    # `herdr plugin install/link` back to the declared set on each activation.
    home.activation.herdrPluginRegistry = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      run install -Dm644 ${registry} "$HOME/.config/herdr/plugins.json"
    '';
  };
}
