# Declarative service users/groups for lldap, see ensure.py.
# Only import on the primary: the replica's database is overwritten by the
# sync and changes there would be lost.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.lldap;

  spec = pkgs.writers.writeJSON "lldap-ensure.json" {
    groups = cfg.ensureGroups;
    users = lib.mapAttrs (_: u: {
      inherit (u) email displayName groups;
      passwordFile = u.passwordFile;
    }) cfg.ensureUsers;
  };

in
{
  options.services.lldap = {
    ensureGroups = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Groups to create. Removed again from lldap once dropped here.";
    };
    ensureUsers = lib.mkOption {
      default = { };
      description = "Service users to create. Removed again from lldap once dropped here.";
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }:
          {
            options = {
              email = lib.mkOption {
                type = lib.types.str;
                default = "${name}@localhost";
              };
              displayName = lib.mkOption {
                type = lib.types.str;
                default = name;
              };
              passwordFile = lib.mkOption {
                type = lib.types.nullOr lib.types.path;
                default = null;
                description = "File with the plaintext password. Only applied when a login with it fails.";
              };
              groups = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
                description = "Exact set of group memberships (others are removed).";
              };
            };
          }
        )
      );
    };
  };

  config = lib.mkIf (cfg.ensureUsers != { } || cfg.ensureGroups != [ ]) {
    systemd.services.lldap-ensure = {
      description = "Reconcile declarative lldap users and groups";
      after = [ "lldap.service" ];
      requires = [ "lldap.service" ];
      wantedBy = [ "multi-user.target" ];
      # lldap needs a moment to open the http port after start.
      preStart = ''
        for _ in $(seq 30); do
          ${pkgs.curl}/bin/curl -sf -o /dev/null ${lib.escapeShellArg "http://127.0.0.1:${toString cfg.settings.http_port}/health"} && exit 0
          sleep 1
        done
        exit 1
      '';
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        StateDirectory = "lldap-ensure";
        LoadCredential = [
          "admin-password:${config.clan.core.vars.generators.lldap.files.admin-password.path}"
        ];
        ExecStart = lib.concatStringsSep " " [
          "${pkgs.python3.interpreter} ${./ensure.py}"
          "--port ${toString cfg.settings.http_port}"
          "--admin-user ${cfg.settings.ldap_user_dn}"
          "--admin-password-file %d/admin-password"
          "--spec ${spec}"
          "--state /var/lib/lldap-ensure/state.json"
          "--set-password-bin ${cfg.package}/bin/lldap_set_password"
        ];
      };
    };
  };
}
