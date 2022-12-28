{ pkgs, ... }: {
  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql_14;
  services.postgresql.settings = {
    max_connections = "300";
    shared_buffers = "80MB";
  };
  services.postgresqlBackup.enable = true;
}
