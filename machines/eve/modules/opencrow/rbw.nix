# Mock rbw that returns secrets from systemd credential files.
# Domain modules register entries via services.opencrow.rbwEntries,
# mapping "rbw get <args>" to credential file names.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  mockRbw = pkgs.writeShellScriptBin "rbw" (
    ''
      if [ "$1" != "get" ]; then
        echo "mock rbw: only 'get' is supported" >&2
        exit 1
      fi
      shift
      key="$*"
      case "$key" in
    ''
    + lib.concatStrings (
      lib.mapAttrsToList (args: credFile: ''
        ${lib.escapeShellArg args})
          cat "/run/credentials/opencrow.service/${credFile}"
          ;;
      '') config.services.opencrow.rbwEntries
    )
    + ''
        *)
          echo "mock rbw: unknown entry: $key" >&2
          exit 1
          ;;
      esac
    ''
  );
in
{
  options.services.opencrow.rbwEntries = lib.mkOption {
    type = lib.types.attrsOf lib.types.str;
    default = { };
    description = ''
      Map from rbw key name (the argument to "rbw get") to the
      corresponding systemd credential file name.
    '';
  };

  config.services.opencrow.extraPackages = [ mockRbw ];
}
