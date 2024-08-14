{ lib, pkgs, ... }:
{
  options.clan.core = {
    clanName = lib.mkOption {
      type = lib.types.str;
      description = ''
        the name of the clan
      '';
    };
    machineIcon = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = ''
        the location of the machine icon
      '';
    };
    machineDescription = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        the description of the machine
      '';
    };
    clanDir = lib.mkOption {
      type = lib.types.path;
      default = ./.;
      description = ''
        the location of the flake repo, used to calculate the location of facts and secrets
      '';
    };
    clanIcon = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      description = ''
        the location of the clan icon
      '';
    };
    machineName = lib.mkOption {
      type = lib.types.str;
      default = "nixos";
      description = ''
        the name of the machine
      '';
    };
    clanPkgs = lib.mkOption {
      defaultText = "self.packages.${pkgs.system}";
      internal = true;
    };
  };
}
