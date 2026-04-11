# strfry Nostr relay with optional peer mirroring.
#
# Used by eve (nostr.thalheim.io) and eva (nostr2.thalheim.io). Clients
# add *both* URLs and talk to them in parallel; the `strfry router`
# stream keeps the two LMDB databases converging in the background so it
# doesn't matter which relay an event was originally published to.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.mic92-strfry;
  port = 7777;
in
{
  options.services.mic92-strfry = {
    enable = lib.mkEnableOption "strfry Nostr relay";

    domain = lib.mkOption {
      type = lib.types.str;
      description = "Public hostname the relay is reachable at.";
    };

    description = lib.mkOption {
      type = lib.types.str;
      default = "A general-purpose Nostr relay";
    };

    acme = lib.mkOption {
      # eve serves everything off a DNS-01 wildcard, eva requests
      # per-vhost certs via HTTP-01. Let the caller pick instead of
      # hard-coding either.
      type = lib.types.attrs;
      example = {
        useACMEHost = "thalheim.io";
      };
      description = "nginx vhost ACME attrs (useACMEHost or enableACME).";
    };

    syncPeers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "wss://nostr.thalheim.io" ];
      description = ''
        Relays to keep a persistent bidirectional `strfry router` stream
        to. Each side actively pushes and pulls, so neither depends on
        the other being up to eventually backfill missed events.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.etc."strfry.conf".text = ''
      db = "/var/lib/strfry/"

      relay {
        bind = "127.0.0.1"
        port = ${toString port}

        info {
          name = "Nostr Relay on ${cfg.domain}"
          description = "${cfg.description}"
          contact = ""
        }

        nofiles = 0
        maxWebsocketPayloadSize = 131072
        autoPingSeconds = 55
        enableTCPKeepalive = false

        writePolicy {
          plugin = ""
        }

        logging {
          # The default (invalidEvents = true) logs every rejected event,
          # which on a public relay means ~95k "ephemeral event expired"
          # lines per 10 minutes from clients replaying stale kind-2xxxx
          # events. journald rate-limits on top. Nothing actionable.
          invalidEvents = false
        }
      }
    '';

    systemd.services.strfry = {
      description = "strfry Nostr relay";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.strfry}/bin/strfry --config=/etc/strfry.conf relay";
        Restart = "on-failure";
        RestartSec = 5;

        DynamicUser = true;
        StateDirectory = "strfry";

        # WebSocket connections are long-lived; each client holds an fd.
        # strfry.conf sets nofiles=0 so it inherits this limit instead
        # of trying to raise it itself (which DynamicUser can't do).
        LimitNOFILE = 65536;

        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        ReadWritePaths = [ "/var/lib/strfry" ];
      };
    };

    # Router shares the same LMDB env as the relay; strfry's locking is
    # designed for multiple processes on one db, this is the upstream-
    # supported deployment for sync.
    environment.etc."strfry-router.conf" = lib.mkIf (cfg.syncPeers != [ ]) {
      text = ''
        connectionTimeout = 20

        streams {
          ${lib.concatImapStringsSep "\n" (i: url: ''
            peer${toString i} {
              dir = "both"
              urls = [ "${url}" ]
            }
          '') cfg.syncPeers}
        }
      '';
    };

    systemd.services.strfry-router = lib.mkIf (cfg.syncPeers != [ ]) {
      description = "strfry sync stream";
      wantedBy = [ "multi-user.target" ];
      after = [
        "network-online.target"
        "strfry.service"
      ];
      wants = [ "network-online.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.strfry}/bin/strfry --config=/etc/strfry.conf router /etc/strfry-router.conf";
        Restart = "always";
        RestartSec = 10;

        DynamicUser = true;
        StateDirectory = "strfry";

        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        ReadWritePaths = [ "/var/lib/strfry" ];
      };
    };

    services.nginx.virtualHosts.${cfg.domain} = cfg.acme // {
      forceSSL = true;
      locations."/".extraConfig = ''
        proxy_pass http://127.0.0.1:${toString port};
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;

        # Nostr clients hold WebSocket connections open indefinitely.
        # nginx defaults to 60s read/send timeout which would drop any
        # connection that goes quiet between strfry's 55s pings.
        proxy_read_timeout 3600s;
        proxy_send_timeout 3600s;
      '';
    };
  };
}
