{ config, lib, pkgs, ... }:

{
  services.buildbot-master = {
    enable = true;
    masterCfg = ./master.py;
    dbUrl = "postgresql://@/buildbot";
    pythonPackages = ps: [ ps.requests ps.treq ps.psycopg2 ];
  };
  services.buildbot-worker = {
    enable = true;
    workerUser = "eve";
    adminMessage = "Mic92";
    home = "/var/lib/buildbot-worker";
    hostMessage = "A mighty Hetzner server";
    user = "buildbot-worker";
    group = "buildbot-worker";
    packages = [ pkgs.git pkgs.nix ];
    workerPassFile = config.sops.secrets.buildbot-nix-worker-password.path;
  };
  systemd.services.buildbot-master = {
    environment = {
      PORT   = "1810";
      DB_URL = config.services.buildbot-master.dbUrl;
      GITHUB_OAUTH_ID = "d1b24258af1abc157934";
      GITHUB_ADMINS = "Mic92,mic92,Jörg Thalheim <joerg@thalheim.io>";
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
  users.users.buildbot-worker = {
    description = "Buildbot Worker User.";
    isSystemUser = true;
    createHome = true;
    home = "/var/lib/buildbot-worker";
    group = "buildbot-worker";
    useDefaultShell = true;
  };
  users.groups.buildbot-worker = {};

  sops.secrets = {
    github-token = {};
    github-webhook-secret = {};
    github-oauth-secret = {};
    github-workers = {};
    buildbot-nix-worker-password.owner = "buildbot-worker";
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
    locations."/".proxyPass = "http://127.0.0.1:1810/";
    locations."/sse" = {
      proxyPass = "http://127.0.0.1:1810/sse/";
      # proxy buffering will prevent sse to work
      extraConfig = "proxy_buffering off;";
    };
    locations."/ws" = {
      proxyPass = "http://127.0.0.1:1810/ws";
      proxyWebsockets = true;
      # raise the proxy timeout for the websocket
      extraConfig = "proxy_read_timeout 6000s;";
    };
  };
}
