{ config, ... }: {
  services.gogs = {
    enable = true;
    database = {
      type = "postgres";
      host = "172.23.75.10";
      port = 5432;
      passwordFile = "/run/keys/gogs-database";
    };
    domain = "git.higgsboson.tk";
    rootUrl = "https://git.higgsboson.tk";
    extraConfig = ''
      [mailer]
      ENABLED = true
      FROM = gogs@higgsboson.tk
      USER = gogs@higgsboson.tk
      PASSWD = #dbpass#

      [service]
      DISABLE_REGISTRATION = true
    '';
  };

  # for database key
  users.users.gogs.extraGroups = [ "keys" ];
  systemd.services.gogs.serviceConfig.SupplementaryGroups = [ "keys" ];

  deployment.keys."gogs-database" = {
    keyFile = ../secrets/gogs-database;
    user = config.services.gogs.user;
  };
}
