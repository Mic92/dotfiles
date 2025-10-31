{ pkgs, ... }:
{
  services.freshrss = {
    enable = true;
    package = pkgs.freshrss;

    # Use http_auth for Authelia integration via Remote-User header
    authType = "http_auth";

    # Domain configuration
    virtualHost = "rss.devkid.net";
    baseUrl = "https://rss.devkid.net";

    defaultUser = "admin";
    language = "en";

    # PostgreSQL database configuration
    database = {
      type = "pgsql";
      host = "/run/postgresql";
      port = null; # Use Unix socket
      user = "freshrss";
      name = "freshrss";
      passFile = null; # Unix socket authentication
    };

    dataDir = "/var/lib/freshrss";
  };

  # PostgreSQL database setup
  services.postgresql = {
    ensureDatabases = [ "freshrss" ];
    ensureUsers = [
      {
        name = "freshrss";
        ensureDBOwnership = true;
      }
    ];
  };

  # PHP-FPM pool settings
  services.phpfpm.pools.freshrss.settings = {
    "pm" = "dynamic";
    "pm.max_children" = 32;
    "pm.start_servers" = 2;
    "pm.min_spare_servers" = 2;
    "pm.max_spare_servers" = 5;
    "pm.process_idle_timeout" = "10s";
    "pm.max_requests" = 500;
  };

  # Nginx configuration
  services.nginx = {
    # Redirect rss.thalheim.io to rss.devkid.net
    virtualHosts."rss.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      globalRedirect = "rss.devkid.net";
    };

    # Main virtualhost with Authelia forward-auth
    # Note: The FreshRSS module already configures this virtualHost,
    # so we extend it with Authelia integration
    virtualHosts."rss.devkid.net" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;

      # Redirect to login page on 401
      extraConfig = ''
        error_page 401 =302 https://auth.devkid.net/?rd=$scheme://$http_host$request_uri;
      '';

      # Add Authelia auth to the root location
      locations."/".extraConfig = ''
        # Authelia forward-auth
        auth_request /authelia;
        auth_request_set $user $upstream_http_remote_user;
      '';

      # Add Authelia auth to PHP locations and pass REMOTE_USER
      locations."~ ^.+?\\.php(/.*)?$".extraConfig = ''
        # Authelia forward-auth
        auth_request /authelia;
        auth_request_set $user $upstream_http_remote_user;

        # Pass REMOTE_USER to PHP-FPM for http_auth
        fastcgi_param REMOTE_USER $user;
      '';

      # Authelia forward-auth verification endpoint
      locations."/authelia" = {
        proxyPass = "http://127.0.0.1:9091/api/verify";
        extraConfig = ''
          internal;

          # Set authentication request headers
          proxy_set_header X-Original-URL $scheme://$http_host$request_uri;
          proxy_set_header X-Original-Method $request_method;
          proxy_set_header X-Forwarded-Method $request_method;
          proxy_set_header X-Forwarded-Proto $scheme;
          proxy_set_header X-Forwarded-Host $http_host;
          proxy_set_header X-Forwarded-Uri $request_uri;
          proxy_set_header X-Forwarded-For $remote_addr;
          proxy_set_header Content-Length "";
          proxy_set_header Connection "";

          # Don't forward the request body
          proxy_pass_request_body off;

          # Timeout settings
          proxy_connect_timeout 5s;
          proxy_send_timeout 5s;
          proxy_read_timeout 5s;
        '';
      };
    };
  };
}
