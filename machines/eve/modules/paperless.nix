{
  config,
  lib,
  pkgs,
  ...
}:
let
  hostname = "paperless.thalheim.io";
  apiHostname = "paperless-api.thalheim.io";
  opencrowPermsApp = ./paperless-perms;
  cfg = config.services.paperless;
  paperlessPythonPath = "${cfg.package.python.pkgs.makePythonPath cfg.package.propagatedBuildInputs}:${cfg.package}/lib/paperless-ngx/src:${opencrowPermsApp}";
in
{
  services.paperless = {
    enable = true;
    address = "127.0.0.1";
    port = 28981;
    dataDir = "/var/lib/paperless";
    mediaDir = "/var/lib/paperless/media";
    consumptionDir = "/var/lib/paperless/consume";
    consumptionDirIsPublic = true;
    passwordFile = config.clan.core.vars.generators.paperless.files.paperless-admin-password.path;
    domain = hostname;
    settings = {
      PAPERLESS_DBHOST = "/run/postgresql";
      PAPERLESS_DBNAME = "paperless";
      PAPERLESS_DBUSER = "paperless";
      PAPERLESS_SECRET_KEY = config.clan.core.vars.generators.paperless.files.paperless-secret-key.path;
      PAPERLESS_ADMIN_USER = "admin";
      PAPERLESS_ADMIN_MAIL = "joerg@thalheim.io";
      PAPERLESS_OCR_LANGUAGE = "deu+eng";
      PAPERLESS_TIME_ZONE = "Europe/Berlin";
      PAPERLESS_TASK_WORKERS = 2;
      PAPERLESS_THREADS_PER_WORKER = 1;
      PAPERLESS_WEBSERVER_WORKERS = 2;
      PAPERLESS_CONSUMER_RECURSIVE = true;
      PAPERLESS_CONSUMER_IGNORE_PATTERN = [
        ".DS_STORE/*"
        "desktop.ini"
        "Thumbs.db"
      ];
      PAPERLESS_FILENAME_FORMAT = "{created_year}/{correspondent}/{title}";
      PAPERLESS_FILENAME_FORMAT_REMOVE_NONE = true;
      PAPERLESS_ALLOWED_HOSTS = "${hostname},${apiHostname}";
      PAPERLESS_CORS_ALLOWED_HOSTS = "https://${hostname},https://${apiHostname}";
      PAPERLESS_CSRF_TRUSTED_ORIGINS = "https://${hostname},https://${apiHostname}";
      PAPERLESS_APPS = "paperless_perms.apps.PaperlessPermsConfig";
      PAPERLESS_ENABLE_HTTP_REMOTE_USER = true;
      PAPERLESS_HTTP_REMOTE_USER_HEADER_NAME = "HTTP_REMOTE_USER";
      PAPERLESS_LOGOUT_REDIRECT_URL = "/"; # Redirect to root after logout
    };
  };

  clan.core.vars.generators.paperless = {
    files.paperless-admin-password = { };
    files.paperless-secret-key = { };
    migrateFact = "paperless";
    runtimeInputs = with pkgs; [
      openssl
      xkcdpass
    ];
    script = ''
      xkcdpass -n 3 -d - | tr -d '\n' > $secrets/paperless-admin-password
      openssl rand -hex 32 | tr -d '\n' > $secrets/paperless-secret-key
    '';
  };

  # Add the opencrow_perms Django app to PYTHONPATH for all paperless
  # services so post_save signals fire in every worker process.
  systemd.services.paperless-web.environment.PYTHONPATH = lib.mkForce paperlessPythonPath;
  systemd.services.paperless-task-queue.environment.PYTHONPATH = paperlessPythonPath;
  systemd.services.paperless-consumer.environment.PYTHONPATH = paperlessPythonPath;
  systemd.services.paperless-scheduler.environment.PYTHONPATH = paperlessPythonPath;

  services.postgresql.ensureDatabases = [ "paperless" ];
  services.postgresql.ensureUsers = [
    {
      name = "paperless";
      ensureDBOwnership = true;
    }
  ];

  services.nginx.virtualHosts.${hostname} = {
    useACMEHost = "thalheim.io";
    forceSSL = true;

    # Redirect to Authelia login on 401
    extraConfig = ''
      error_page 401 =302 https://auth.thalheim.io/?rd=$scheme://$http_host$request_uri;
    '';

    # Authelia forward-auth verification endpoint
    locations."/authelia" = {
      proxyPass = "http://127.0.0.1:9091/api/verify";
      extraConfig = ''
        internal;
        proxy_set_header X-Original-URL $scheme://$http_host$request_uri;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header X-Forwarded-Host $http_host;
        proxy_set_header X-Forwarded-For $remote_addr;
        proxy_set_header Content-Length "";
        proxy_pass_request_body off;
      '';
    };

    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.services.paperless.port}";
      proxyWebsockets = true;
      recommendedProxySettings = true;
      extraConfig = ''
        client_max_body_size 200M;
        proxy_read_timeout 300s;
        proxy_connect_timeout 300s;
        proxy_send_timeout 300s;

        # Forward auth request to Authelia
        auth_request /authelia;
        auth_request_set $user $upstream_http_remote_user;

        # Pass authenticated username to paperless
        # Django automatically prefixes headers with HTTP_
        proxy_set_header Remote-User $user;

        # Remove the Authorization header to prevent conflicts
        proxy_set_header Authorization "";
      '';
    };
  };

  # Separate API domain without PAM authentication
  services.nginx.virtualHosts.${apiHostname} = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.services.paperless.port}";
      proxyWebsockets = true;
      recommendedProxySettings = true;
      extraConfig = ''
        client_max_body_size 200M;
        proxy_read_timeout 300s;
        proxy_connect_timeout 300s;
        proxy_send_timeout 300s;

        # No PAM authentication for API domain - uses token auth
        # Explicitly clear Remote-User header to prevent header injection
        proxy_set_header Remote-User "";
      '';
    };
  };
}
