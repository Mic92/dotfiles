# Replica side (eva) of the lldap postgres replication.
#
# Pulls a pg_dump from eve over a command-restricted ssh account (see
# sync-primary.nix) and restores it into the local lldap database in a
# single transaction. lldap reads the database per request, so no restart
# is needed after a restore. Local writes are overwritten on the next
# sync: this instance is a read replica by convention, analogous to the
# old openldap syncrepl consumer.
#
# NOTE: both machines must share the same lldap key_seed, otherwise the
# replicated password hashes are unusable here.
{ pkgs, config, ... }:
let
  syncKey = config.clan.core.vars.generators.lldap-sync;
in
{
  clan.core.vars.generators.lldap-sync = {
    share = true;
    files.ssh-private-key = {
      secret = true;
      owner = "postgres";
    };
    files.ssh-public-key.secret = false;
    runtimeInputs = [ pkgs.openssh ];
    script = ''
      ssh-keygen -t ed25519 -N "" -f "$out/ssh-private-key" -C "lldap-sync"
      mv "$out/ssh-private-key.pub" "$out/ssh-public-key"
    '';
  };

  services.postgresql = {
    enable = true;
    ensureDatabases = [ "lldap" ];
    ensureUsers = [
      {
        name = "lldap";
        ensureDBOwnership = true;
      }
    ];
  };

  systemd.services.lldap-sync = {
    after = [
      "postgresql.service"
      "network-online.target"
    ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Type = "oneshot";
      # postgres superuser via peer auth; restores must be able to drop
      # and recreate objects owned by the lldap role.
      User = "postgres";
    };
    path = [
      pkgs.openssh
      config.services.postgresql.package
    ];
    script = ''
      ssh -i ${syncKey.files.ssh-private-key.path} \
          -o StrictHostKeyChecking=accept-new \
          lldap-sync@eve.r \
        | psql -v ON_ERROR_STOP=1 --single-transaction -d lldap
    '';
  };

  systemd.timers.lldap-sync = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "5min";
      OnUnitActiveSec = "15min";
      RandomizedDelaySec = "1min";
    };
  };
}
