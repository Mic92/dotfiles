{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [ ../../../nixosModules/radicle-node.nix ];

  # HTTP gateway configuration
  services.radicle.httpd = {
    enable = true;
    nginx = {
      serverName = "radicle.thalheim.io";
      useACMEHost = "thalheim.io";
      forceSSL = true;
      quic = true;
    };
  };

  # Web-specific settings (node settings come from shared module)
  services.radicle.settings.web.pinned.repositories = [
    "rad:z3gpeDzWxqV8iBEN8RcJNZEPVWmJf"
    "rad:z2dqRKkK5yu89w3CMX2dVsYrRwvFk"
  ];

  # Serve radicle-explorer as static files (override upstream's proxy)
  services.nginx.virtualHosts."radicle.thalheim.io" = {
    # Reset listen to use eve's default (with specific IPs) - upstream sets generic ones
    listen = lib.mkForce [
      {
        addr = "[::1]";
        port = 443;
        ssl = true;
      }
      {
        addr = "[42:0:3c46:70c7:8526:2adf:7451:8bbb]";
        port = 80;
      }
      {
        addr = "[42:0:3c46:70c7:8526:2adf:7451:8bbb]";
        port = 443;
        ssl = true;
      }
      {
        addr = "[${config.networking.eve.ipv6.address}]";
        port = 80;
      }
      {
        addr = "[${config.networking.eve.ipv6.address}]";
        port = 443;
        ssl = true;
      }
      {
        addr = "0.0.0.0";
        port = 80;
      }
      {
        addr = "0.0.0.0";
        port = 443;
        ssl = true;
      }
    ];

    # Set root at virtualHost level for static file serving
    root = lib.mkForce "${pkgs.radicle-explorer.withConfig {
      preferredSeeds = [
        {
          hostname = "radicle.thalheim.io";
          port = 443;
          scheme = "https";
        }
      ];
    }}";

    # Override "/" to serve static files instead of proxying
    locations."/" = {
      proxyPass = lib.mkForce null;
      tryFiles = "$uri $uri/ /index.html =404";
      extraConfig = ''
        expires 1h;
        add_header Cache-Control "public, immutable";
      '';
    };
    # API endpoints go to radicle-httpd
    locations."/api/" = {
      proxyPass = "http://127.0.0.1:8080";
      recommendedProxySettings = true;
    };
  };
}
