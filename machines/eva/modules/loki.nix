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
  inherit (config.services.loki) dataDir;
in
{
  systemd.tmpfiles.rules = [
    "d ${dataDir}/rules 0700 loki loki - -"
    # ruler local storage layout is <dir>/<tenant>/*.yml; with
    # auth_enabled=false the tenant is the literal string "fake". Ownership
    # must match the parent dir or tmpfiles refuses the L+ with an
    # "unsafe path transition" error.
    "d ${dataDir}/ruler 0755 loki loki - -"
    "d ${dataDir}/ruler/fake 0755 loki loki - -"
    "L+ ${dataDir}/ruler/fake/ruler.yml - - - - ${rulerFile}"
  ];
  services.loki = {
    enable = true;
    configuration = {
      auth_enabled = false;
      server = {
        http_listen_address = "127.0.0.1";
        http_listen_port = 3100;
        grpc_listen_address = "127.0.0.1";
        log_level = "warn";
      };
      common = {
        path_prefix = dataDir;
        storage.filesystem = {
          chunks_directory = "${dataDir}/chunks";
          rules_directory = "${dataDir}/rules";
        };
        replication_factor = 1;
        ring.kvstore.store = "inmemory";
        ring.instance_addr = "127.0.0.1";
        # grpc only listens on loopback; without this the query-frontend
        # advertises the first private interface address (e.g. phantun1)
        # and the querier's dial is refused, hanging every query.
        instance_addr = "127.0.0.1";
      };
      frontend.address = "127.0.0.1";

      limits_config = {
        retention_period = "120h";
        ingestion_burst_size_mb = 16;
        reject_old_samples_max_age = "12h";
        # ~35 own hosts + doctor cluster + makefu. A single misbehaving
        # shipper must not be able to 429 everyone else (see fluent-bit
        # collapse_unit_instance), but the default 5000 leaves no headroom.
        max_global_streams_per_user = 20000;
        split_queries_by_interval = "24h";
      };

      # retention with tsdb is enforced by the compactor, not table_manager
      compactor = {
        retention_enabled = true;
        compaction_interval = "10m";
        working_directory = "${dataDir}/compactor";
        retention_delete_delay = "2h";
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
          local.directory = "${dataDir}/ruler";
        };
        rule_path = "${dataDir}/rules";
        alertmanager_url = "http://alertmanager.r";
      };

      query_range.cache_results = true;
    };
  };

  # htpasswd with one entry per shipper (own hosts, doctor cluster, makefu's
  # hosts via retiolum). Plaintext for external parties lives in
  # promtail-<name>-password so it can be handed over / rotated separately.
  sops.secrets.promtail-nginx-password.owner = "nginx";
  systemd.services.loki.restartTriggers = [ rulerFile ];

  security.acme.certs."loki.r".server = config.retiolum.ca.acmeURL;
  services.nginx = {
    enable = true;
    virtualHosts."loki.r" = {
      serverName = "loki.r";
      enableACME = true;
      addSSL = true;
      basicAuthFile = config.sops.secrets.promtail-nginx-password.path;
      locations."/" = {
        proxyPass = "http://127.0.0.1:3100";
        # grafana (on eve) live-tails over websocket; long range queries
        proxyWebsockets = true;
        extraConfig = ''
          proxy_read_timeout 1800s;
          access_log off;
        '';
      };
      # unauthenticated for telegraf's http_response check
      locations."/ready" = {
        proxyPass = "http://127.0.0.1:3100";
        extraConfig = ''
          auth_basic off;
          access_log off;
        '';
      };
    };
  };
}
