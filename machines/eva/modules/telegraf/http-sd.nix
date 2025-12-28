{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.telegraf.httpSd;
  outputDir = "/var/lib/telegraf/http-sd";

  generateConfig = pkgs.runCommand "telegraf-http-sd-generate" { } ''
    mkdir -p $out/bin
    echo '#!${pkgs.python3}/bin/python3' > $out/bin/telegraf-http-sd-generate
    cat ${./http-sd-generate.py} >> $out/bin/telegraf-http-sd-generate
    chmod +x $out/bin/telegraf-http-sd-generate
  '';

  # Shell script to fetch and generate all configs
  generatorScript = pkgs.writeShellScript "telegraf-http-sd-generator" ''
    set -euo pipefail

    mkdir -p "${outputDir}"

    ${lib.concatMapStringsSep "\n" (target: ''
      echo "Fetching ${target.name} targets from ${target.url}..."
      ${generateConfig}/bin/telegraf-http-sd-generate "${target.name}" "${target.url}" "${outputDir}" \
        || echo "Failed to fetch ${target.name}, keeping existing config"
    '') (lib.attrValues cfg.targets)}

    echo "HTTP SD generation complete"
  '';
in
{
  options.services.telegraf.httpSd = {
    targets = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            name = lib.mkOption {
              type = lib.types.str;
              description = "Name identifier for this target set";
            };
            url = lib.mkOption {
              type = lib.types.str;
              description = "HTTP SD URL to fetch targets from";
            };
          };
        }
      );
      default = { };
      description = "HTTP service discovery targets for Telegraf";
    };
  };

  config = lib.mkIf (cfg.targets != { }) {
    # Create the config directory
    systemd.tmpfiles.rules = [
      "d ${outputDir} 0755 telegraf telegraf -"
    ];

    # Service to generate config from HTTP SD
    systemd.services.telegraf-http-sd = {
      description = "Generate Telegraf config from HTTP Service Discovery";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      before = [ "telegraf.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "oneshot";
        ExecStart = generatorScript;
        # Reload telegraf if running (+ prefix runs as root, --no-block avoids deadlock on boot)
        ExecStartPost = "+${pkgs.systemd}/bin/systemctl try-reload-or-restart --no-block telegraf.service";
        User = "telegraf";
        Group = "telegraf";
        Restart = "on-failure";
        RestartSec = "30s";
      };
    };

    # Timer to periodically refresh targets
    systemd.timers.telegraf-http-sd = {
      description = "Refresh Telegraf HTTP SD targets";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "5min";
        OnUnitActiveSec = "5min";
        RandomizedDelaySec = "30s";
      };
    };

    # Tell Telegraf to load configs from the http-sd directory
    # Config path is /var/run/telegraf/config.toml when environmentFiles is used
    systemd.services.telegraf.serviceConfig.ExecStart = lib.mkForce [
      ""
      "${pkgs.telegraf}/bin/telegraf -config /var/run/telegraf/config.toml -config-directory ${outputDir}"
    ];
  };
}
