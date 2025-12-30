# Module that provides environment.casks option for installing macOS apps
# via ditto with efficient marker-based caching.
{
  config,
  lib,
  ...
}:
let
  cfg = config.environment;

  # Generate the sync script for all apps
  syncScript = lib.concatMapStringsSep "\n" (pkg: ''
    sync_app "${pkg}"
  '') cfg.casks;
in
{
  options.environment.casks = lib.mkOption {
    type = lib.types.listOf lib.types.package;
    default = [ ];
    description = ''
      List of macOS application packages to install via ditto.
      Uses marker-based caching to only copy when the store path changes.
      Apps are installed to /Applications/Nix Casks.
    '';
  };

  config = lib.mkIf (cfg.casks != [ ]) {
    # Efficient marker-based app syncing - only copies when store path changes
    system.activationScripts.postActivation.text = lib.mkAfter ''
      echo "setting up /Applications/Nix Casks..." >&2
      targetDir='/Applications/Nix Casks'
      markerDir="$targetDir/.sources"
      mkdir -p "$targetDir" "$markerDir"

      # Track which apps we want to keep
      declare -A wantedApps

      sync_app() {
        local src="$1"

        for app in "$src/Applications/"*.app; do
          [[ -e "$app" ]] || continue
          local appName
          appName=$(basename "$app")
          local dest="$targetDir/$appName"
          local marker="$markerDir/$appName"

          wantedApps["$appName"]=1

          if [[ ! -f "$marker" ]] || [[ "$(cat "$marker")" != "$src" ]]; then
            echo "Syncing $appName..." >&2
            chmod -R u+w "$dest" 2>/dev/null || true
            rm -rf "$dest"
            /usr/bin/ditto "$app" "$dest"
            echo "$src" > "$marker"
          fi
        done
      }

      ${syncScript}

      # Remove apps no longer in the list
      for app in "$targetDir/"*.app; do
        [[ -e "$app" ]] || continue
        appName=$(basename "$app")
        if [[ -z "''${wantedApps[$appName]:-}" ]]; then
          echo "Removing stale $appName..." >&2
          chmod -R u+w "$app" 2>/dev/null || true
          rm -rf "$app"
          rm -f "$markerDir/$appName"
        fi
      done
    '';
  };
}
