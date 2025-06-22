{
  config,
  pkgs,
  lib,
  ...
}:
let
  hostname = "paperless.thalheim.io";
  ldapConf = pkgs.writeText "paperless-ldap.conf" ''
    base dc=eve
    host localhost:389
    pam_login_attribute mail
    pam_filter objectClass=paperlessUser
  '';
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
    settings = {
      PAPERLESS_DBHOST = "/run/postgresql";
      PAPERLESS_DBNAME = "paperless";
      PAPERLESS_DBUSER = "paperless";
      PAPERLESS_URL = "https://${hostname}";
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
      PAPERLESS_ALLOWED_HOSTS = hostname;
      PAPERLESS_CORS_ALLOWED_HOSTS = "https://${hostname}";
      PAPERLESS_CSRF_TRUSTED_ORIGINS = "https://${hostname}";
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

  # Setup script to create default group and add signal handler
  systemd.services.paperless-group-setup = {
    description = "Setup paperless editors group";
    after = [ "paperless-web.service" ];
    wantedBy = [ "multi-user.target" ];
    script = ''
      # Create editors group with permissions
      cd /var/lib/paperless
      ${pkgs.util-linux}/bin/runuser -u paperless -- /run/current-system/sw/bin/paperless-manage shell <<EOF
      from django.contrib.auth.models import Group, Permission, User
      from django.contrib.contenttypes.models import ContentType
      from django.contrib.auth.signals import user_logged_in
      from django.db.models.signals import post_save
      from django.dispatch import receiver

      # Create or get the editors group
      editors_group, created = Group.objects.get_or_create(name="editors")
      if created:
          print("Created editors group")
      else:
          print("Editors group already exists")

      # Grant all view and change permissions (but not delete or admin)
      permissions_to_add = []
      for ct in ContentType.objects.all():
          permissions = Permission.objects.filter(
              content_type=ct,
              codename__regex=r"^(view|change|add)_"
          )
          permissions_to_add.extend(permissions)

      editors_group.permissions.set(permissions_to_add)
      print(f"Added {len(permissions_to_add)} permissions to editors group")

      # Add all existing remote users to editors group
      for user in User.objects.filter(username__contains="@"):
          if not user.groups.filter(name="editors").exists():
              user.groups.add(editors_group)
              print(f"Added existing user {user.username} to editors group")
      EOF
    '';
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
  };

  # PAM configuration for paperless LDAP authentication
  security.pam.services.paperless.text = ''
    auth required ${pkgs.pam_ldap}/lib/security/pam_ldap.so config=${ldapConf}
    account required ${pkgs.pam_ldap}/lib/security/pam_ldap.so config=${ldapConf}
  '';

  # Ensure nginx has PAM module
  services.nginx.package = lib.mkDefault (
    pkgs.nginxQuic.override { modules = [ pkgs.nginxModules.pam ]; }
  );

  services.nginx.virtualHosts.${hostname} = {
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

        # PAM authentication
        auth_pam "Paperless LDAP Login";
        auth_pam_service_name "paperless";

        # Pass authenticated username to paperless
        # Django automatically prefixes headers with HTTP_
        proxy_set_header Remote-User $remote_user;

        # Remove the Authorization header to prevent conflicts
        proxy_set_header Authorization "";
      '';
    };
    locations."~ ^/api/" = {
      proxyPass = "http://127.0.0.1:${toString config.services.paperless.port}";
      proxyWebsockets = true;
      recommendedProxySettings = true;
      priority = 100;  # Higher priority than "/"
      extraConfig = ''
        client_max_body_size 200M;
        proxy_read_timeout 300s;
        proxy_connect_timeout 300s;
        proxy_send_timeout 300s;

        # No PAM authentication for API endpoints - uses token auth
        # Explicitly clear Remote-User header to prevent header injection
        proxy_set_header Remote-User "";
      '';
    };
  };
}
