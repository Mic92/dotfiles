{
  services.nextcloud = {
    enable = true;
    hostName = "cloud.thalheim.io";

    nginx.enable = true;
    caching.apcu = true;

    config = {
      dbtype = "pgsql";
      dbname = "owncloud";
      dbtableprefix = "oc_";
      dbuser = "owncloud";
      dbhost = "172.23.75.10";
      dbpassFile = "/run/keys/nextcloud-db-password";
      adminuser = "nextcloudadmin";
      adminpassFile = "/run/keys/nextcloud-admin-password";
      extraTrustedDomains = [
        "cloud.higgsboson.tk"
        "pim.devkid.net"
      ];
    };
  };

  deployment.keys = {
    "nextcloud-db-password" = {
      keyFile = ../secrets/nextcloud-db-password;
      user = "nextcloud";
    };
    "nextcloud-admin-password" = {
      keyFile = ../secrets/nextcloud-admin-password;
      user = "nextcloud";
    };
  };

  users.users.nextcloud.extraGroups = [ "keys" ];
  systemd.services.nextcloud.serviceConfig.SupplementaryGroups = [ "keys" ];

  services.nginx = {
    virtualHosts."cloud.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "cloud.thalheim.io";
    };
    virtualHosts."cloud.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      serverAliases = [ "pim.devkid.net" ];
    };
  };
}
