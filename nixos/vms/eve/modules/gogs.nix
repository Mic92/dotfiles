{ config, ... }: {
  services.gogs = {
    enable = true;
    database = {
      type = "postgres";
      host = "/tmp";
      port = 5432;
    };
    domain = "git.higgsboson.tk";
    rootUrl = "https://git.higgsboson.tk";
    extraConfig = ''
      [mailer]
      ENABLED = true
      FROM = gogs@higgsboson.tk
      USER = gogs@higgsboson.tk
      HOST = mail.higgsboson.tk:587
      PASSWD = #dbpass#

      [service]
      DISABLE_REGISTRATION = true
    '';
  };

  services.nginx = {
    virtualHosts."git.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/".extraConfig = ''
        proxy_pass http://localhost:3000;
      '';
    };
    virtualHosts."git.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "git.thalheim.io";
    };
  };

  services.netdata.httpcheck.checks.gogs = {
    url = "https://git.thalheim.io";
    regex = "Gogs";
  };
 
  # for database key
  systemd.services.gogs.serviceConfig.SupplementaryGroups = [ "keys" ];
}
