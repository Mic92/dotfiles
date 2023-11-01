{ config
, pkgs
, ...
}:
let
  rulerConfig = {
    groups = [
      {
        name = "general";
        rules = [
          {
            alert = "Coredumps";
            # filter out failed build gitlab CI runner, users or nix build sandboxes
            expr = ''sum by (host) (count_over_time({unit=~"systemd-coredump.*"} !~ "(/runner/_work|/home|/build|/scratch)" |~ "core dumped"[10m])) > 0'';
            for = "10s";
            annotations.description = ''{{ $labels.instance }} {{ $labels.coredump_unit }} core dumped in last 10min.'';
          }
        ];
      }
    ];
  };

  rulerDir = pkgs.writeTextDir "ruler/ruler.yml" (builtins.toJSON rulerConfig);
in
{
  systemd.tmpfiles.rules = [
    "d /var/lib/loki 0700 loki loki - -"
    "d /var/lib/loki/ruler 0700 loki loki - -"
  ];
  services.loki = {
    enable = true;
    configuration = {
      # Basic stuff
      auth_enabled = false;
      server = {
        http_listen_port = 3100;
        log_level = "warn";
      };
      common.instance_interface_names = [ "eth0" ];

      # Distributor
      distributor.ring.kvstore.store = "inmemory";

      # Ingester
      ingester = {
        lifecycler.ring = {
          kvstore.store = "inmemory";
          replication_factor = 1;
        };
        chunk_encoding = "snappy";
        # Disable block transfers on shutdown
        max_transfer_retries = 0;
      };

      # Storage
      storage_config = {
        boltdb.directory = "/var/lib/loki/boltdb";
        filesystem.directory = "/var/lib/loki/storage";
      };

      limits_config.retention_period = "120h";
      limits_config.ingestion_burst_size_mb = 16;
      limits_config.reject_old_samples = true;
      limits_config.reject_old_samples_max_age = "12h";

      # Table manager
      table_manager = {
        retention_deletes_enabled = true;
        retention_period = "120h";
      };

      compactor = {
        retention_enabled = true;
        compaction_interval = "10m";
        shared_store = "filesystem";
        working_directory = "/var/lib/loki/compactor";
        delete_request_cancel_period = "10m"; # don't wait 24h before processing the delete_request
        retention_delete_delay = "2h";
        retention_delete_worker_count = 150;
      };

      # Schema
      schema_config.configs = [
        {
          from = "2020-11-08";
          store = "boltdb";
          object_store = "filesystem";
          schema = "v11";
          index.prefix = "index_";
          index.period = "120h";
        }
      ];


      ruler = {
        storage = {
          type = "local";
          local.directory = rulerDir;
        };
        rule_path = "/var/lib/loki/ruler";
        alertmanager_url = "http://alertmanager.r";
        ring.kvstore.store = "inmemory";
      };

      query_range.cache_results = true;
      limits_config.split_queries_by_interval = "24h";
    };
  };

  sops.secrets.promtail-nginx-password.owner = "nginx";

  security.acme.certs."loki.r".server = config.retiolum.ca.acmeURL;
  services.nginx = {
    enable = true;
    virtualHosts.loki = {
      serverName = "loki.r";
      enableACME = true;
      addSSL = true;
      locations."/" = {
        proxyWebsockets = true;
        extraConfig = ''
          auth_basic "Loki password";
          auth_basic_user_file ${config.sops.secrets.promtail-nginx-password.path};

          proxy_read_timeout 1800s;
          proxy_redirect off;
          proxy_connect_timeout 1600s;

          access_log off;
          proxy_pass http://127.0.0.1:3100;
        '';
      };
      locations."/ready" = {
        proxyWebsockets = true;
        extraConfig = ''
          auth_basic off;
          access_log off;
          proxy_pass http://127.0.0.1:3100;
        '';
      };
    };
  };

  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 80 ];
}
