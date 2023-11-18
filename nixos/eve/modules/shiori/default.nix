{ lib, pkgs, ... }: {
  services.shiori.enable = true;
  services.shiori.port = 4378;
  services.shiori.package = pkgs.shiori.overrideAttrs (_: {
    patches = [
      ./0001-set-saner-postgresql-connection-default-and-make-use.patch
    ];
  });
  systemd.services.shiori.environment = {
    SHIORI_PG_HOST = "/run/postgresql";
    SHIORI_PG_PORT = "5432";
    SHIORI_PG_USER = "shiori";
    SHIORI_PG_NAME = "shiori";
    SHIORI_DBMS = "postgresql";
  };
  systemd.services.shiori = {
    serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "shiori";
      Group = "shiori";
      RestrictAddressFamilies = [ "AF_UNIX" ];
      BindPaths = [ "/run/postgresql" ];
    };
  };
  users.users.shiori = {
    isSystemUser = true;
    home = "/var/lib/shiori";
    group = "shiori";
  };
  users.groups.shiori = { };

  services.postgresql.ensureDatabases = [ "shiori" ];
  services.postgresql.ensureUsers = [{
    name = "shiori";
    ensureDBOwnership = true;
  }];
  services.nginx.virtualHosts."shiori.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".proxyPass = "http://localhost:4378";
  };
}
