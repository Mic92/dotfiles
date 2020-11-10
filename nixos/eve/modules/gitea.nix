{ config, pkgs, ... }: {
  services.gitea = {
    enable = true;
    database = {
      type = "postgres";
      host = "/run/postgresql";
      port = 5432;
    };
    log.level = "Error";
    domain = "git.thalheim.io";
    rootUrl = "https://git.thalheim.io";
    mailerPasswordFile = config.sops.secrets.gitea-mail.path;
    disableRegistration = false;
    settings.mailer = {
      ENABLED = true;
      FROM = "gitea@thalheim.io";
      USER = "gitea@thalheim.io";
      HOST = "mail.thalheim.io:587";
    };
    settings.server.DISABLE_ROUTER_LOG = true;
  };

  sops.secrets.gitea-mail.owner = config.systemd.services.gitea.serviceConfig.User;

  services.nginx.virtualHosts."git.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://localhost:3000;
    '';
  };

  # for database key
  systemd.services.gitea.serviceConfig.SupplementaryGroups = [ "keys" ];
}
