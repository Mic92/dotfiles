{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.uci.settings;
  settingsFormat = pkgs.formats.json {};
  settingsFile = settingsFormat.generate "settings.json" cfg.settings;
in {
  options.uci = {
    settings = lib.mkOption {
      default = {};
      type = settingsFormat.type;
    };
    secrets.sops.files = lib.mkOption {
      default = [];
      type = lib.types.listOf lib.types.path;
      description = "List of sops files to parse and load. All keys in the provided files are merged into one attrset. Key collisions are ignored.";
    };
  };
}
