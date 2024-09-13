{ config, pkgs, ... }:
{
  services.nextcloud = {
    enable = true;
    hostName = "cloud.thalheim.io";
    https = true;

    caching.apcu = true;

    package = pkgs.nextcloud29;

    config = {
      dbtype = "pgsql";
      dbname = "nextcloud";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql";
      adminuser = "nextcloudadmin";
      adminpassFile = config.sops.secrets.nextcloud-admin-password.path;
    };

    settings.trusted_domains = [ "pim.devkid.net" ];
    settings.dbtableprefix = "oc_";

    poolSettings = {
      "pm" = "ondemand";
      "pm.max_children" = 32;
      "pm.process_idle_timeout" = "10s";
      "pm.max_requests" = 500;
    };
  };

  sops.secrets.nextcloud-admin-password.owner = "nextcloud";

  services.nginx.virtualHosts."cloud.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    serverAliases = [ "pim.devkid.net" ];
  };
}
