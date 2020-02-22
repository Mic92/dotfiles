{
  services.postgresql.enable = true;
  services.postgresqlBackup.enable = true;

  environment.etc."netdata/python.d/postgres.conf".text = ''
    socket:
      name     : 'local'
      user     : 'netdata'
      database : 'postgres'
  '';

  services.postgresql.ensureUsers = [{
    name = "netdata";
    ensurePermissions."ALL TABLES IN SCHEMA public" = "SELECT";
  }];
}
