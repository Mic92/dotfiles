{ config, pkgs, ... }:
{
  services.shiori.enable = true;
  services.shiori.port = 4378;
  services.shiori.environmentFile = config.clan.core.facts.services.shiori.secret.shiori-env.path;
  services.shiori.databaseUrl = "postgres:///shiori?host=/run/postgresql";

  clan.core.facts.services.shiori = {
    secret."shiori-env" = { };
    generator.path = [ pkgs.openssl ];
    generator.script = ''
      printf "SHIORI_HTTP_SECRET_KEY=%s\n" "$(openssl rand -hex 16)" > $secrets/shiori-env
    '';
  };

  services.postgresql.ensureDatabases = [ "shiori" ];
  services.postgresql.ensureUsers = [
    {
      name = "shiori";
      ensureDBOwnership = true;
    }
  ];
  services.nginx.virtualHosts."shiori.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".proxyPass = "http://localhost:4378";
  };
}
