# Primary side (eve) of the lldap postgres replication.
#
# lldap has no built-in replication, so eva pulls a pg_dump of the lldap
# database over ssh on a timer (see sync-replica.nix). This module provides
# a dedicated, command-restricted ssh account that can only produce that
# dump.
{ pkgs, config, ... }:
let
  # Shared generator: the private key is deployed on the replica (eva),
  # the public key is referenced here for the forced command.
  syncKey = config.clan.core.vars.generators.lldap-sync;

  dumpScript = pkgs.writeShellApplication {
    name = "lldap-dump";
    runtimeInputs = [ config.services.postgresql.package ];
    text = ''
      exec pg_dump --clean --if-exists lldap
    '';
  };
in
{
  clan.core.vars.generators.lldap-sync = {
    share = true;
    files.ssh-private-key = {
      secret = true;
      deploy = false; # only needed on the replica
    };
    files.ssh-public-key.secret = false;
    runtimeInputs = [ pkgs.openssh ];
    script = ''
      ssh-keygen -t ed25519 -N "" -f "$out/ssh-private-key" -C "lldap-sync"
      mv "$out/ssh-private-key.pub" "$out/ssh-public-key"
    '';
  };

  users.groups.lldap-sync = { };
  users.users.lldap-sync = {
    isSystemUser = true;
    group = "lldap-sync";
    shell = "${pkgs.bash}/bin/bash";
    openssh.authorizedKeys.keys = [
      ''restrict,command="${dumpScript}/bin/lldap-dump" ${syncKey.files.ssh-public-key.value}''
    ];
  };

  services.postgresql.ensureUsers = [ { name = "lldap-sync"; } ];

  # pg_dump needs SELECT on all tables of the lldap database.
  systemd.services.lldap-sync-grant = {
    after = [ "postgresql.service" ];
    requires = [ "postgresql.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      User = "postgres";
      RemainAfterExit = true;
    };
    script = ''
      ${config.services.postgresql.package}/bin/psql -d lldap \
        -c 'GRANT pg_read_all_data TO "lldap-sync"'
    '';
  };
}
