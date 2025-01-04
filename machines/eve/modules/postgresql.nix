{
  lib,
  config,
  pkgs,
  ...
}:
{
  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql_14;
  services.postgresql.settings = {
    max_connections = "300";
    shared_buffers = "80MB";
  };
  services.postgresqlBackup.enable = true;
  services.postgresqlBackup.backupAll = true;
  services.postgresqlBackup.compression = "zstd";

  environment.systemPackages = [
    (
      let
        # XXX specify the postgresql package you'd like to upgrade to.
        # Do not forget to list the extensions you need.
        newPostgres = pkgs.postgresql_15.withPackages (_pp: [
          # pp.plv8
        ]);
        cfg = config.services.postgresql;
      in
      pkgs.writeScriptBin "upgrade-pg-cluster" ''
        set -eux
        # XXX it's perhaps advisable to stop all services that depend on postgresql
        systemctl stop postgresql

        export NEWDATA="/var/lib/postgresql/${newPostgres.psqlSchema}"

        export NEWBIN="${newPostgres}/bin"

        export OLDDATA="${cfg.dataDir}"
        export OLDBIN="${cfg.package}/bin"

        install -d -m 0700 -o postgres -g postgres "$NEWDATA"
        cd "$NEWDATA"
        sudo -u postgres $NEWBIN/initdb -D "$NEWDATA" ${lib.escapeShellArgs cfg.initdbArgs}

        sudo -u postgres $NEWBIN/pg_upgrade \
          --old-datadir "$OLDDATA" --new-datadir "$NEWDATA" \
          --old-bindir $OLDBIN --new-bindir $NEWBIN \
          "$@"
      ''
    )
  ];
}
