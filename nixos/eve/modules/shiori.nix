{
  services.shiori.enable = true;
  services.shiori.port = 4378;
  systemd.services.shiori.environment = {
    SHIORI_PG_HOST = "/run/postgresql";
  };
  services.postgresql.ensureDatabases = [ "shiori" ];
  services.postgresql.ensureUsers = [{
    name = "shiori";
    ensurePermissions."DATABASE shiori" = "ALL PRIVILEGES";
  }];
  services.nginx.virtualHosts."shiori.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".proxyPass = "http://localhost:4378";
  };
}
