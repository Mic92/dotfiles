# Web frontend for the radicle-mirror node: radicle-explorer as static
# files, radicle-httpd serving the API from the mirror's storage.
{ pkgs, config, ... }:
let
  domain = "radicle.thalheim.io";
  apiPort = 8080;
  meiliUrl = "http://127.0.0.1:${toString config.services.meilisearch.listenPort}";
  # Debounce reindexing until the fix is in a radicle-httpd release.
  radicle-httpd = pkgs.radicle-httpd.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ./radicle-search-debounce.patch ];
  });
in
{
  # repo search index for the explorer
  services.meilisearch.enable = true;

  systemd.services.radicle-search = {
    description = "Radicle search indexer";
    wantedBy = [ "multi-user.target" ];
    after = [
      "radicle-mirror.service"
      "meilisearch.service"
    ];

    serviceConfig = {
      ExecStart = "${radicle-httpd}/bin/radicle-search";
      Environment = [
        "RAD_HOME=/var/lib/radicle-mirror/rad"
        "RADICLE_SEARCH_MEILI_URL=${meiliUrl}"
      ];
      Restart = "on-failure";
      RestartSec = 5;

      # share the dynamic user with radicle-mirror for storage access
      DynamicUser = true;
      User = "radicle-mirror";
      StateDirectory = "radicle-mirror";

      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      PrivateDevices = true;
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectControlGroups = true;
      RestrictAddressFamilies = [
        "AF_INET"
        "AF_INET6"
        "AF_UNIX"
      ];
      RestrictNamespaces = true;
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      SystemCallArchitectures = "native";
    };
  };
  systemd.services.radicle-httpd = {
    description = "Radicle HTTP gateway";
    wantedBy = [ "multi-user.target" ];
    after = [ "radicle-mirror.service" ];

    serviceConfig = {
      ExecStart = "${radicle-httpd}/bin/radicle-httpd --listen 127.0.0.1:${toString apiPort}";
      Environment = [
        "RAD_HOME=/var/lib/radicle-mirror/rad"
        "RADICLE_SEARCH_URL=${meiliUrl}"
      ];
      Restart = "on-failure";
      RestartSec = 5;

      # share the dynamic user with radicle-mirror for storage access
      DynamicUser = true;
      User = "radicle-mirror";
      StateDirectory = "radicle-mirror";

      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      PrivateDevices = true;
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectControlGroups = true;
      RestrictAddressFamilies = [
        "AF_INET"
        "AF_INET6"
        "AF_UNIX"
      ];
      RestrictNamespaces = true;
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      SystemCallArchitectures = "native";
    };
  };

  services.nginx.virtualHosts.${domain} = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    quic = true;

    root = "${pkgs.radicle-explorer.withConfig {
      preferredSeeds = [
        {
          hostname = domain;
          port = 443;
          scheme = "https";
        }
      ];
    }}";

    locations."/" = {
      tryFiles = "$uri $uri/ /index.html =404";
      extraConfig = ''
        expires 1h;
        add_header Cache-Control "public, immutable";
      '';
    };
    locations."/api/" = {
      proxyPass = "http://127.0.0.1:${toString apiPort}";
      recommendedProxySettings = true;
    };
    # Raw files and tarballs (/raw/<rid>/archive/<ref>)
    locations."/raw/" = {
      proxyPass = "http://127.0.0.1:${toString apiPort}";
      recommendedProxySettings = true;
    };
    # Git smart-http clone. The rad: prefix and .git suffix are both
    # optional, aliases like /heartwood.git work too.
    locations."~ ^/((rad:)?z[a-zA-Z0-9]+|[^/]+\\.git)(/|$)" = {
      proxyPass = "http://127.0.0.1:${toString apiPort}";
      recommendedProxySettings = true;
      extraConfig = ''
        proxy_buffering off;
        proxy_request_buffering off;
        client_max_body_size 0;
      '';
    };
  };
}
