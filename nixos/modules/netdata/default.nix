{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.netdata;
in {
  imports = [ ./options.nix ];

  krops.secrets = (if cfg.stream.role == "master" then {
    # Generate <UUID> with `uuidgen`:
    # [<UUID>]
    #     enabled = yes
    #     health enabled by default = yes
    netdata-stream-master = {
      path = "/etc/netdata/stream.conf";
      owner = "netdata";
      sourcePath = toString <shared-secrets> + "/netdata-stream-master";
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
      sourcePath = toString <shared-secrets> + "/netdata-stream-slave";
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
    // (silenceAlerts "udp_errors" [ "ipv4_udperrors_last_collected_secs" ])
    // (silenceAlerts "tcp_resets" [ "ipv4_tcphandshake_last_collected_secs" ]);

  systemd.services.netdata.restartTriggers = [
    config.environment.etc."netdata/python.d/httpcheck.conf".source
    config.environment.etc."netdata/python.d/portcheck.conf".source
  ];
  # TODO create /etc/netdata
}
