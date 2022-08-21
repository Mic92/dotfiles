{
  config,
  lib,
  pkgs,
  ...
}: {

  sops.secrets.headscale.owner = "headscale";
  services.headscale = {
    enable = true;
    serverUrl = "https://krebscale.thalheim.io";

    privateKeyFile = config.sops.secrets.headscale.path;

    settings = {
      db_type = "postgres";
      db_host = "/run/postgresql";
      db_user = "headscale";
      db_name = "headscale";

      grpc_listen_addr = "127.0.0.1:50443";
      grpc_allow_insecure = true;

      derp = {
        server = {
          enabled = true;
          region_id = 999;
          region_code = "fra";
          region_name = "Headscale Embedded DERP";
          # udp
          stun_listen_addr = "[::]:3478";
        };
        urls = [
          "https://controlplane.tailscale.com/derpmap/default"
        ];
        auto_update_enabled = "true";
        update_frequency = "24h";
        disable_check_updates = false;
        logtail.enabled = false;
      };
      ip_prefixes = [
        "100.64.0.0/10"
        "fd7a:115c:a1e0::/48"
      ];
      ephemeral_node_inactivity_timeout = "600m";
    };
  };

  # Allow UDP for STUN
  networking.firewall.allowedUDPPorts = [ 3478 ];

  environment.systemPackages = [
    pkgs.headscale
  ];

  services.postgresql.ensureDatabases = ["headscale"];
  services.postgresql.ensureUsers = [
    {
      name = "headscale";
      ensurePermissions."DATABASE headscale" = "ALL PRIVILEGES";
    }
  ];

  services.nginx.virtualHosts."krebscale.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";
    locations = {
      "/headscale." = {
        extraConfig = "grpc_pass grpc://${config.services.headscale.settings.grpc_listen_addr};";
        priority = 1;
      };
      "/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.headscale.port}";
        proxyWebsockets = true;
        extraConfig = ''
          keepalive_requests          100000;
          keepalive_timeout           160s;
          proxy_buffering             off;
          proxy_connect_timeout       75;
          proxy_ignore_client_abort   on;
          proxy_read_timeout          900s;
          proxy_send_timeout          600;
          send_timeout                600;
        '';
        priority = 99;
      };
    };
  };
}
