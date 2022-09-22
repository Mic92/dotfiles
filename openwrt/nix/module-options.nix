{
  pkgs,
  lib,
  ...
}: let
  settingsFormat = pkgs.formats.json {};
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
