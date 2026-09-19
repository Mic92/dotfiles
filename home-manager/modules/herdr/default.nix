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

  # Saved SSH machines (`herdr machine add`) live in a client-side catalog.
  # herdr only requires a stable 32-hex id per entry, so derive it from the
  # profile itself to keep it identical across hosts and rebuilds.
  endpoints = pkgs.writers.writeJSON "herdr-endpoints.json" {
    version = 1;
    ssh = map (m: {
      id = builtins.hashString "md5" "${m.target}\n${m.session}";
      inherit (m) label target session;
      enabled = true;
    }) cfg.machines;
  };
in
{
  options.programs.herdr = {
    machines = lib.mkOption {
      default = [ ];
      description = "SSH machines shown next to Local in the herdr sidebar.";
      type = lib.types.listOf (
        lib.types.submodule (
          { config, ... }:
          {
            options = {
              target = lib.mkOption {
                type = lib.types.str;
                description = "ssh target, passed to remote.ssh_command";
              };
              label = lib.mkOption {
                type = lib.types.str;
                default = config.target;
              };
              session = lib.mkOption {
                type = lib.types.str;
                default = "default";
              };
            };
          }
        )
      );
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
    home.activation.herdrPluginRegistry = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      run install -Dm644 ${registry} "$HOME/.config/herdr/plugins.json"
    '';
    home.activation.herdrMachines = lib.mkIf (cfg.machines != [ ]) (
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        run install -Dm644 ${endpoints} "''${XDG_STATE_HOME:-$HOME/.local/state}/herdr/client/endpoints.json"
      ''
    );
  };
}
