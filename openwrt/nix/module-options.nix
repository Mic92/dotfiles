{ pkgs, lib, ... }:
let
  fileType = lib.types.submodule (
    { name, config, ... }:
    {
      options = {
        text = lib.mkOption {
          type = lib.types.nullOr lib.types.lines;
          default = null;
        };
        source = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = "File or directory. A directory replaces the target directory, so files missing from source are removed.";
        };
        secret = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Key in one of the sops files whose value becomes the file content.";
        };
        mode = lib.mkOption {
          type = lib.types.str;
          default = if config.secret != null then "0600" else "0644";
        };
      };
      config.source = lib.mkIf (config.text != null) (pkgs.writeText (baseNameOf name) config.text);
    }
  );
in
{
  options.uci = {
    settings = lib.mkOption {
      default = { };
      inherit (pkgs.formats.json { }) type;
    };
    secrets.sops.files = lib.mkOption {
      default = [ ];
      type = lib.types.listOf lib.types.path;
      description = "List of sops files to parse and load. All keys in the provided files are merged into one attrset. Key collisions are ignored.";
    };
  };

  options.openwrt = {
    packages = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Installed with apk before files and uci settings, so package defaults cannot overwrite them.";
    };
    files = lib.mkOption {
      type = lib.types.attrsOf fileType;
      default = { };
    };
    rootPasswordSecret = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Key in one of the sops files holding the root password.";
    };
    restartServices = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Init scripts to restart after applying. Services whose uci config changed are reloaded via reload_config anyway.";
    };
  };
}
