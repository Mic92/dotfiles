{
  config,
  pkgs,
  ...
}:
let
  hostname = "paperless.thalheim.io";
  apiHostname = "paperless-api.thalheim.io";
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
      PAPERLESS_ENABLE_HTTP_REMOTE_USER = true;
      PAPERLESS_HTTP_REMOTE_USER_HEADER_NAME = "HTTP_REMOTE_USER";
      PAPERLESS_LOGOUT_REDIRECT_URL = "/"; # Redirect to root after logout
      PAPERLESS_ACCOUNT_DEFAULT_GROUPS = "editors"; # Default group for new users</
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

  services.postgresql.ensureDatabases = [ "paperless" ];
  services.postgresql.ensureUsers = [
    {
      name = "paperless";
      ensureDBOwnership = true;
    }
  ];

  # One-time setup to create editors group with proper permissions
  systemd.services.paperless-init-groups = {
    description = "Initialize paperless editors group and permissions";
    after = [ "paperless-web.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      # Create editors group with permissions
      cd /var/lib/paperless
      ${pkgs.util-linux}/bin/runuser -u paperless -- /run/current-system/sw/bin/paperless-manage shell <<EOF
      from django.contrib.auth.models import Group, Permission
      from django.contrib.contenttypes.models import ContentType

      # Create or get the editors group
      editors_group, created = Group.objects.get_or_create(name="editors")
      if created:
          print("Created editors group")
      else:
          print("Editors group already exists")

      # Grant all view, change, add, and delete permissions (but not admin)
      permissions_to_add = []
      for ct in ContentType.objects.all():
          permissions = Permission.objects.filter(
              content_type=ct,
              codename__regex=r"^(view|change|add|delete)_"
          )
          permissions_to_add.extend(permissions)

      editors_group.permissions.set(permissions_to_add)
      print(f"Added {len(permissions_to_add)} permissions to editors group")
      EOF
    '';
  };

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
