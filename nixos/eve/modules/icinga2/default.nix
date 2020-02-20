{ lib, ... }:
with lib;

{
  imports = [
    ./config.nix
    ./healthchecks.nix
  ];
  options.services.icinga2.extraConfig = mkOption {
    type = types.lines;
    default = "";
    description = "Icinga configuration file";
  };

  options.services.icinga2.healthchecks = mkOption {
    type = types.attrsOf (types.submodule ({ name, ...}: {
      options = {
        name = mkOption {
          type = types.str;
          default = name;
          example = "borgbackup";
          description = ''
            Name of the healthcheck.
            Also generate a password using:
            pass generate -n eve/healthcheck-borgbackup
          '';
         };
      };
    }));
    default = [];
    description = "Icinga health checks";
  };

}
