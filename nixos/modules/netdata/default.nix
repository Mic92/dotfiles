{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.netdata;
in {
  imports = [ ./options.nix ];

  krops.secrets.files = (if cfg.stream.role == "master" then {
    # Generate <UUID> with `uuidgen`:
    # [<UUID>]
    #     enabled = yes
    #     health enabled by default = yes
    netdata-stream-master = {
      path = "/etc/netdata/stream.conf";
      owner = "netdata";
      source-path = toString <shared-secrets> + "/netdata-stream-master";
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
      source-path = toString <shared-secrets> + "/netdata-stream-slave";
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
    silenceAlerts = filename: alerts: {
      "netdata/health.d/${filename}.conf".source = pkgs.runCommand "${filename}.conf" {} ''
        cp ${pkgs.netdata}/lib/netdata/conf.d/health.d/${filename}.conf $out
        ${concatMapStringsSep "\n" (alert: ''
          sed -i -e '/template: ${alert}$/a to: silent' $out
        '') alerts}
      '';
    };
  in {
    "netdata/health.d/httpcheck.conf".text = ''
      # taken from the original but warn only if a request is at least 300ms slow
      template: web_service_slow
      families: *
      on: httpcheck.responsetime
      lookup: average -3m unaligned of time
      units: ms
      every: 10s
      warn: ($this > ($1h_web_service_response_time * 4) && $this > 1000)
      crit: ($this > ($1h_web_service_response_time * 6) && $this > 1000)
      info: average response time over the last 3 minutes, compared to the average over the last hour
      delay: down 5m multiplier 1.5 max 1h
      options: no-clear-notification
      to: webmaster
    '';
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
  } // (silenceAlerts "disks" [ "10min_disk_backlog" "10min_disk_utilization" ])
    // (silenceAlerts "tcp_listen" [ "1m_tcp_syn_queue_cookies" ])
    // (silenceAlerts "net" [
      "inbound_packets_dropped"
      "inbound_packets_dropped_ratio"
    ])
    // (silenceAlerts "netfilter" [ "netfilter_last_collected_secs" ])
    // (silenceAlerts "udp_errors" [ "ipv4_udperrors_last_collected_secs" ])
    // (silenceAlerts "tcp_resets" [ "ipv4_tcphandshake_last_collected_secs" ]);

  systemd.services.netdata.restartTriggers = [
    config.environment.etc."netdata/python.d/httpcheck.conf".source
    config.environment.etc."netdata/python.d/portcheck.conf".source
  ];
  # TODO create /etc/netdata
}
