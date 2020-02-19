{
  services.nextcloud = {
    enable = true;
    hostName = "cloud.thalheim.io";

    nginx.enable = true;
    caching.apcu = true;

    config = {
      dbtype = "pgsql";
      dbname = "nextcloud";
      dbtableprefix = "oc_";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql";
      adminuser = "nextcloudadmin";
      adminpassFile = "/run/keys/nextcloud-admin-password";
      extraTrustedDomains = [
        "cloud.higgsboson.tk"
        "pim.devkid.net"
      ];
    };

    poolSettings = {
      "pm" = "ondemand";
      "pm.max_children" = 32;
      "pm.process_idle_timeout" = "10s";
      "pm.max_requests" = 500;
    };
  };

  krops.secrets.files.nextcloud-admin-password.owner = "nextcloud";

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

  services.netdata.httpcheck.checks.nextcloud = {
    url = "https://cloud.thalheim.io/login";
    regex = "Nextcloud";
  };

  services.openldap.extraConfig = ''
    attributetype ( 1.3.6.1.4.1.39430.1.1.1
        NAME 'ownCloudQuota'
        DESC 'User Quota (e.g. 15 GB)'
        SYNTAX '1.3.6.1.4.1.1466.115.121.1.15' )

    objectclass ( 1.3.6.1.4.1.39430.1.2.1
        NAME 'ownCloud'
        DESC 'ownCloud LDAP Schema'
        AUXILIARY
        MUST ( mail $ userPassword )
        MAY ( ownCloudQuota ) )

  '';
  services.icinga2.extraConfig = ''
    apply Service "Nextcloud v4 (eve)" {
      import "eve-http4-service"
      vars.http_vhost = "cloud.thalheim.io"
      vars.http_uri = "/login"
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "Nextcloud v6 (eve)" {
      import "eve-http6-service"
      vars.http_vhost = "cloud.thalheim.io"
      vars.http_uri = "/login"
      assign where host.name == "eve.thalheim.io"
    }
  '';
}
