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
      record["unit"] = unit
      record["host"] = record["_HOSTNAME"]

      -- coredump enrichment so the loki ruler alert (`|~ "core dumped"`) keeps firing
      local cgroup = record["COREDUMP_CGROUP"]
      if cgroup ~= nil then
        record["coredump_unit"] = string.match(cgroup, "([^/]+)$")
        local exe = record["COREDUMP_EXE"] or "?"
        local uid = record["COREDUMP_UID"] or "?"
        local gid = record["COREDUMP_GID"] or "?"
        local cmd = record["COREDUMP_CMDLINE"] or "?"
        record["MESSAGE"] = string.format(
          "%s core dumped (user: %s/%s, command: %s)",
          exe, uid, gid, cmd
        )
      end

      -- emit only the fields we care about; loki will turn host/unit/coredump_unit
      -- into stream labels and the remaining MESSAGE becomes the log line
      -- (drop_single_key=on)
      local out = {
        MESSAGE = record["MESSAGE"],
        host = record["host"],
        unit = record["unit"],
        coredump_unit = record["coredump_unit"],
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
          }
        ];
        filters = [
          {
            name = "lua";
            match = "journal";
            script = "${luaFilter}";
            call = "process";
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
            label_keys = "$host,$unit,$coredump_unit";
            remove_keys = lib.concatStringsSep "," [
              "host"
              "unit"
              "coredump_unit"
            ];
            line_format = "key_value";
            drop_single_key = "on";
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
