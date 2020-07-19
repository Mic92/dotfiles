{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.netdata;
in {
  imports = [ ./options.nix ];

  sops.secrets = (if cfg.stream.role == "master" then {
    # Generate <UUID> with `uuidgen`:
    # [<UUID>]
    #     enabled = yes
    #     health enabled by default = yes
    netdata-stream-master = {
      path = "/etc/netdata/stream.conf";
      owner = "netdata";
      sopsFile = ../../secrets-sops/secrets.yaml;
    };
  } else {
    # Generate <UUID> with `uuidgen`:
    # [stream]
    #     enabled = yes
    #     destination = [42:4992:6a6d:a00::1]:19999
    #     api key = <UUID>
    netdata-stream-slave = {
      path = "/etc/netdata/stream.conf";
      owner = "netdata";
      sopsFile = ../../secrets-sops/secrets.yaml;
    };
  });

  users.users.netdata.extraGroups = [ "keys" ];

  services.netdata = {
    enable = true;
    config = {
      global = {
        "bind to" = "0.0.0.0:19999 [::]:19999";
        "error log" = "stderr";
        "update every" = "5";
      };
      health.enable = if cfg.stream.role == "master" then "yes" else "no";
     };
   };

  services.netdata.portcheck.checks.openssh.port = (lib.head config.services.openssh.ports);

  networking.firewall.allowedTCPPorts = [ 19999 ];

  environment.etc = let
    silenceAlertScript = pkgs.writers.writePython3 "silence-alerts" {} ''
      import sys
      import re

      alerts = set(sys.argv[3:])
      disable_alert = False
      with open(sys.argv[1]) as src, open(sys.argv[2], "w") as dst:
          for line in src:
              match = re.match(r".*template: (\S+).*", line)
              if match:
                  disable_alert = match.group(1) in alerts
              if disable_alert and re.match(r".*to: .*", line):
                  dst.write("to: silent\n")
              else:
                  dst.write(line)
    '';

    silenceAlerts = filename: alerts: {
      "netdata/health.d/${filename}.conf".source = pkgs.runCommand "${filename}.conf" {} ''
        exec ${silenceAlertScript} ${pkgs.netdata}/lib/netdata/conf.d/health.d/${filename}.conf $out \
          ${concatStringsSep " " alerts}
      '';
    };
  in {
    "netdata/python.d/httpcheck.conf".text = ''
      update_every: 30
      ${lib.concatStringsSep "\n" (mapAttrsToList (site: options:
      ''
        ${site}:
          url: '${options.url}'
          ${optionalString (options.regex != null) "regex: '${options.regex}'"}
          status_accepted: [ ${lib.concatStringsSep " " (map toString options.statusAccepted) } ]
        '') cfg.httpcheck.checks)
        }
      '';
    "netdata/python.d/portcheck.conf".text = ''
      ${lib.concatStringsSep "\n" (mapAttrsToList (service: options:
      ''
        ${service}:
          host: '${options.host}'
          port: ${toString options.port}
        '') cfg.portcheck.checks)
        }
      '';
  } // (silenceAlerts "httpcheck" [ "web_service_slow" ])
    // (silenceAlerts "disks" [ "10min_disk_backlog" "10min_disk_utilization" ])
    // (silenceAlerts "tcp_listen" [ "1m_tcp_syn_queue_cookies" ])
    // (silenceAlerts "net" [
      "inbound_packets_dropped"
      "inbound_packets_dropped_ratio"
    ])
    // (silenceAlerts "netfilter" [ "netfilter_last_collected_secs" ])
    // (silenceAlerts "udp_errors" [ "ipv4_udperrors_last_collected_secs" "1m_ipv4_udp_receive_buffer_errors" ])
    // (silenceAlerts "tcp_resets" [ "ipv4_tcphandshake_last_collected_secs" ]);

  systemd.services.netdata.restartTriggers = [
    config.environment.etc."netdata/python.d/httpcheck.conf".source
    config.environment.etc."netdata/python.d/portcheck.conf".source
  ];
  # TODO create /etc/netdata
}
