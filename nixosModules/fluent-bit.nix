{
  config,
  lib,
  pkgs,
  ...
}:
let
  # Re-use the existing shared `promtail` var so the already-provisioned
  # loki basic-auth password keeps working without re-prompting on every host.
  passwordFile = config.clan.core.vars.generators.promtail.files."password".path;

  # Stream labels (low cardinality, indexed by loki).
  labelKeys = [
    "host"
    "unit"
    "coredump_unit"
  ];

  # Per-line structured metadata (high cardinality, queryable but not indexed).
  # Loki 3.x stores these alongside the log line so `{host="eve"} | priority <= "3"`
  # works without blowing up the label index.
  metadataKeys = [
    "priority"
    "syslog_identifier"
    "pid"
    "container_name"
  ];

  # fluent-bit has no expression language comparable to promtail's
  # pipeline_stages, so the bits that the loki ruler relies on (the
  # `core dumped` rewrite, the `unit`/`coredump_unit` labels and the
  # noise drops) are reproduced in a small Lua filter instead.
  luaFilter = pkgs.writeText "fluent-bit-journal.lua" ''
    function process(tag, timestamp, record)
      local msg = record["MESSAGE"]

      -- noise that used to be handled by promtail `drop` stages
      if msg ~= nil then
        if string.find(msg, "ignored inotify event for", 1, true)
          or string.find(msg, "hwmon hwmon1: Undervoltage detected!", 1, true)
          or string.find(msg, "hwmon hwmon1: Voltage normalised", 1, true)
          or string.find(msg, "refused connection: IN=", 1, true)
        then
          return -1, timestamp, record
        end
      end

      -- unit label: fall back to transport (audit/kernel) like promtail did
      local unit = record["_SYSTEMD_UNIT"]
      if unit == nil or unit == "" then
        unit = record["_TRANSPORT"]
      end
      -- collapse session-1234.scope -> session.scope to keep label cardinality low
      if unit ~= nil then
        unit = string.gsub(unit, "^session%-%d+%.scope$", "session.scope")
      end

      -- coredump enrichment so the loki ruler alert (`|~ "core dumped"`) keeps firing
      local coredump_unit
      local cgroup = record["COREDUMP_CGROUP"]
      if cgroup ~= nil then
        coredump_unit = string.match(cgroup, "([^/]+)$")
        local exe = record["COREDUMP_EXE"] or "?"
        local uid = record["COREDUMP_UID"] or "?"
        local gid = record["COREDUMP_GID"] or "?"
        local cmd = record["COREDUMP_CMDLINE"] or "?"
        msg = string.format(
          "%s core dumped (user: %s/%s, command: %s)",
          exe, uid, gid, cmd
        )
      end

      -- emit only the fields we care about: stream labels + structured metadata
      -- + MESSAGE. After label_keys/remove_keys/structured_metadata are applied
      -- only MESSAGE remains in the record so drop_single_key=on yields the
      -- plain log line.
      local out = {
        MESSAGE = msg,
        -- stream labels
        host = record["_HOSTNAME"],
        unit = unit,
        coredump_unit = coredump_unit,
        -- structured metadata
        priority = record["PRIORITY"],
        syslog_identifier = record["SYSLOG_IDENTIFIER"],
        pid = record["_PID"],
        container_name = record["CONTAINER_NAME"],
      }
      return 2, timestamp, out
    end
  '';
in
{
  clan.core.vars.generators.promtail = {
    files."password" = { };
    prompts.password.type = "hidden";
    share = true;
  };

  services.fluent-bit = {
    enable = true;
    settings = {
      service = {
        flush = 1;
        log_level = "warn";
        # expose /api/v2/metrics/prometheus on the same port promtail used so
        # the existing telegraf scrape keeps working unchanged
        http_server = "on";
        http_listen = "127.0.0.1";
        http_port = 9080;
        # filesystem-backed buffering so logs survive a loki outage / network
        # blip on roaming hosts (matchbox, laptops) instead of being dropped
        # once the in-memory buffer fills.
        "storage.path" = "/var/lib/fluent-bit/storage";
        "storage.sync" = "normal";
        "storage.max_chunks_up" = 64;
        "storage.backlog.mem_limit" = "16M";
      };
      pipeline = {
        inputs = [
          {
            name = "systemd";
            tag = "journal";
            db = "/var/lib/fluent-bit/journal.db";
            read_from_tail = "on";
            max_entries = 1000;
            strip_underscores = "off";
            "storage.type" = "filesystem";
          }
        ];
        filters = [
          {
            # fold multi-line stacktraces (go/python/java) into a single loki
            # entry so `|= "panic"` / `|= "Traceback"` returns the whole trace.
            name = "multiline";
            match = "journal";
            "multiline.key_content" = "MESSAGE";
            "multiline.parser" = "go,python,java";
          }
          {
            name = "lua";
            match = "journal";
            script = "${luaFilter}";
            call = "process";
          }
          {
            # eve's strfry alone produces ~10k lines/min; cap any single
            # journal stream so it cannot eat the 120h retention budget.
            name = "throttle";
            match = "journal";
            rate = 200;
            window = 60;
            interval = "1s";
            print_status = false;
          }
        ];
        outputs = [
          {
            name = "loki";
            match = "journal";
            host = "loki.r";
            port = 80;
            uri = "/loki/api/v1/push";
            http_user = "promtail@thalheim.io";
            http_passwd = "\${LOKI_PASSWORD}";
            labels = "job=systemd-journal";
            label_keys = lib.concatMapStringsSep "," (k: "$" + k) labelKeys;
            structured_metadata = lib.concatMapStringsSep "," (k: "${k}=$" + k) metadataKeys;
            remove_keys = lib.concatStringsSep "," (labelKeys ++ metadataKeys);
            line_format = "key_value";
            drop_single_key = "on";
            # cap on-disk backlog for the loki output so a long outage on a
            # small box (matchbox) cannot fill the disk.
            "storage.total_limit_size" = "256M";
          }
        ];
      };
    };
  };

  systemd.services.fluent-bit = {
    serviceConfig = {
      StateDirectory = "fluent-bit";
      RuntimeDirectory = "fluent-bit";
      LoadCredential = "loki-password:${passwordFile}";
      EnvironmentFile = "-/run/fluent-bit/env";
    };
    # fluent-bit's loki output cannot read the password from a file, so turn the
    # credential into an env var before the main process starts.
    preStart = ''
      umask 0077
      printf 'LOKI_PASSWORD=%s\n' "$(cat "$CREDENTIALS_DIRECTORY/loki-password")" \
        > /run/fluent-bit/env
    '';
  };
}
