{ config, lib, pkgs, ... }:

{
  services.buildbot-master = {
    enable = true;
    masterCfg = ./master.py;
    dbUrl = "postgresql://@/buildbot";
    pythonPackages = ps: [ ps.requests ps.treq ps.psycopg2 ];
  };
  systemd.services.buildbot-master = {
    environment = {
      PORT   = "1810";
      DB_URL = config.services.buildbot-master.dbUrl;
      GITHUB_OAUTH_ID = "d1b24258af1abc157934";
      GITHUB_ADMINS = "Mic92";
    };
    serviceConfig = {
      LoadCredential = [
        "github-token:${config.sops.secrets.github-token.path}"
        "github-webhook-secret:${config.sops.secrets.github-webhook-secret.path}"
        "github-oauth-secret:${config.sops.secrets.github-oauth-secret.path}"
        "github-workers:${config.sops.secrets.github-workers.path}"
      ];
    };
  };

  sops.secrets = {
    github-token = {};
    github-webhook-secret = {};
    github-oauth-secret = {};
    github-workers = {};
  };
  services.postgresql = {
    ensureDatabases = ["buildbot"];
    ensureUsers = [
      {
        name = "buildbot";
        ensurePermissions = {
          "DATABASE buildbot" = "ALL PRIVILEGES";
        };
      }
    ];
  };

  services.nginx.virtualHosts."buildbot.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";
    locations."/".proxyPass = ''
      http://127.0.0.1:1810
    '';
  };
}
