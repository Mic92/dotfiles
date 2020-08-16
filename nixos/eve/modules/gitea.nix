{ config, pkgs, ... }: {
  services.gitea = {
    enable = true;
    database = {
      type = "postgres";
      host = "/run/postgresql";
      port = 5432;
    };
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
  };

  sops.secrets.gitea-mail.owner = config.systemd.services.gitea.serviceConfig.User;

  services.nginx.virtualHosts."git.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://localhost:3000;
    '';
  };

  services.netdata.httpcheck.checks.gitea = {
    url = "https://git.thalheim.io";
    regex = "Gitea";
  };

  # for database key
  systemd.services.gitea.serviceConfig.SupplementaryGroups = [ "keys" ];

  services.openldap.settings.children."cn={1}gitlab,cn=schema".attrs =  {
    cn = "{1}gitlab";
    objectClass = "olcSchemaConfig";
    olcObjectClasses = [''( 1.3.6.1.4.1.28293.1.2.4 NAME 'gitlab'
      SUP uidObject AUXILIARY
      DESC 'Added to an account to allow gitlab access'
      MUST (mail))
    ''];
  };
}
