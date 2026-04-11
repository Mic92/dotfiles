{ config, pkgs, ... }:
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
            annotations.description = "{{ $labels.instance }} {{ $labels.coredump_unit }} core dumped in last 10min.";
          }
          {
            # dead-man switch: every host running fluent-bit should produce at
            # least *some* journal lines within 15m. Catches broken shippers,
            # dead retiolum links and wedged journald long before telegraf
            # notices anything.
            alert = "LogIngestStalled";
            expr = ''sum by (host) (count_over_time({job="systemd-journal"}[15m])) unless sum by (host) (count_over_time({job="systemd-journal"}[5m]))'';
            for = "5m";
            annotations.description = "{{ $labels.host }} has not shipped any journal logs to loki in the last 5 minutes (but did in the last 15m).";
          }
        ];
      }
      {
        # recording rules: turn log patterns into prometheus series so they
        # can be graphed/alerted on alongside telegraf metrics.
        name = "log-derived-metrics";
        rules = [
          {
            record = "loki:sshd_invalid_user:rate5m";
            expr = ''sum by (host) (rate({job="systemd-journal", unit="sshd.service"} |= "Invalid user" [5m]))'';
          }
        ];
      }
    ];
  };

  rulerFile = pkgs.writeText "ruler.yml" (builtins.toJSON rulerConfig);
in
{
  systemd.tmpfiles.rules = [
    "d /var/lib/loki 0700 loki loki - -"
    "d /var/lib/loki/rules 0700 loki loki - -"
    # ruler local storage layout is <dir>/<tenant>/*.yml; with
    # auth_enabled=false the tenant is the literal string "fake". Ownership
    # must match the parent dir or tmpfiles refuses the L+ with an
    # "unsafe path transition" error.
    "d /var/lib/loki/ruler 0755 loki loki - -"
    "d /var/lib/loki/ruler/fake 0755 loki loki - -"
    "L+ /var/lib/loki/ruler/fake/ruler.yml - - - - ${rulerFile}"
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
      common = {
        path_prefix = config.services.loki.dataDir;
        storage.filesystem = {
          chunks_directory = "${config.services.loki.dataDir}/chunks";
          rules_directory = "${config.services.loki.dataDir}/rules";
        };
        replication_factor = 1;
        ring.kvstore.store = "inmemory";
        ring.instance_addr = "127.0.0.1";
      };

      ingester.chunk_encoding = "snappy";

      limits_config = {
        retention_period = "120h";
        ingestion_burst_size_mb = 16;
        reject_old_samples = true;
        reject_old_samples_max_age = "12h";
      };

      table_manager = {
        retention_deletes_enabled = true;
        retention_period = "120h";
      };

      compactor = {
        retention_enabled = true;
        compaction_interval = "10m";
        working_directory = "${config.services.loki.dataDir}/compactor";
        delete_request_cancel_period = "10m"; # don't wait 24h before processing the delete_request
        retention_delete_delay = "2h";
        retention_delete_worker_count = 150;
        delete_request_store = "filesystem";
      };

      schema_config.configs = [
        {
          from = "2020-11-08";
          store = "tsdb";
          object_store = "filesystem";
          schema = "v13";
          index.prefix = "index_";
          index.period = "24h";
        }
      ];

      ruler = {
        storage = {
          type = "local";
          local.directory = "${config.services.loki.dataDir}/ruler";
        };
        rule_path = "${config.services.loki.dataDir}/rules";
        alertmanager_url = "http://alertmanager.r";
      };

      query_range.cache_results = true;
      limits_config.split_queries_by_interval = "24h";
    };
  };

  sops.secrets.promtail-nginx-password.owner = "nginx";
  systemd.services.loki.restartTriggers = [ rulerFile ];

  security.acme.certs."loki.r".server = config.retiolum.ca.acmeURL;
  services.nginx = {
    enable = true;
    virtualHosts."loki.r" = {
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
