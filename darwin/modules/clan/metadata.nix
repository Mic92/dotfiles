{ lib, pkgs, ... }:
let
  inherit (lib) mkOption types;
in
{
  options.clan.core = {
    settings = mkOption {
      description = ''
        Settings of the clan.

        This is a read-only attribute-set available to the machines of the clan.
      '';
      type = types.submodule {
        options = {
          directory = mkOption {
            type = types.path;
            description = ''
              the location of the flake repo, used to calculate the location of facts and secrets
            '';
          };
          name = lib.mkOption {
            type = lib.types.str;
            description = ''
              the name of the clan
            '';
            # Set by the flake, so it's read-only in the maschine
            readOnly = true;
          };
          icon = lib.mkOption {
            type = lib.types.nullOr lib.types.path;
            description = ''
              the location of the clan icon
            '';
            # Set by the flake, so it's read-only in the maschine
            readOnly = true;
          };
          machine = mkOption {
            description = ''
              Settings of the machine.

              This is a read-only attribute-set available to the machines of the clan.
            '';
            default = { };
            type = types.submodule {
              options = {
                name = mkOption {
                  type = types.str;
                  default = "nixos";
                  description = ''
                    the name of the machine
                  '';
                };
                icon = lib.mkOption {
                  type = lib.types.nullOr lib.types.path;
                  default = null;
                  description = ''
                    the location of the machine icon
                  '';
                };
                description = lib.mkOption {
                  type = lib.types.nullOr lib.types.str;
                  default = null;
                  description = ''
                    the description of the machine
                  '';
                };
              };
            };
          };
        };
      };
    };

    # TODO: Move this into settings.clanPkgs ?
    # This could also be part of the public interface to allow users to override the internal packages
    clanPkgs = lib.mkOption {
      defaultText = "self.packages.${pkgs.system}";
      internal = true;
    };
  };
}
