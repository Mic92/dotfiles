{ lib, ... }:
let
  retiolumOnly = ''
    allow 42::/16;
    deny all;
  '';
in
{
  options.services.nginx.virtualHosts = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule (
        { name, config, ... }:
        let
          serverName = if config.serverName != null then config.serverName else name;
          serverNames = [ serverName ] ++ config.serverAliases;
          retiolumHost = lib.all (lib.hasSuffix ".r") serverNames;
        in
        {
          config.extraConfig = lib.mkIf retiolumHost (lib.mkAfter retiolumOnly);
        }
      )
    );
  };
}
