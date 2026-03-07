# Nostr relay stack: general-purpose relay (strfry) + NIP-29 groups (khatru29)
#
# General relay:
#   wss://nostr.thalheim.io
#   Add as relay in any Nostr client (Damus, Amethyst, Primal, etc.)
#
# NIP-29 groups relay:
#   wss://nostr-groups.thalheim.io
#   Use with group-aware clients (e.g. Groups by max21dev)
#
# Test connectivity:
#   curl -H "Accept: application/nostr+json" https://nostr.thalheim.io/
#   curl -H "Accept: application/nostr+json" https://nostr-groups.thalheim.io/
{
  config,
  pkgs,
  ...
}:
let
  strfryPort = 7777;
  groupsRelayPort = 2929;
  strfryDomain = "nostr.thalheim.io";
  groupsDomain = "nostr-groups.thalheim.io";
  groups-relay = pkgs.callPackage ../../../pkgs/groups-relay { };
in
{
  # ── General relay (strfry) ──────────────────────────────────────────────

  environment.etc."strfry.conf" = {
    text = ''
      db = "/var/lib/strfry/"

      relay {
        bind = "127.0.0.1"
        port = ${toString strfryPort}

        info {
          name = "Nostr Relay on ${strfryDomain}"
          description = "A general-purpose Nostr relay"
          contact = ""
        }

        nofiles = 0
        maxWebsocketPayloadSize = 131072
        autoPingSeconds = 55
        enableTCPKeepalive = false

        writePolicy {
          plugin = ""
        }
      }
    '';
  };

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

  services.nginx.virtualHosts.${strfryDomain} = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://127.0.0.1:${toString strfryPort};
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Proto $scheme;
      proxy_http_version 1.1;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection $connection_upgrade;
    '';
  };

  # ── NIP-29 groups relay (khatru29) ──────────────────────────────────────

  systemd.services.nostr-groups-relay = {
    description = "NIP-29 groups relay (khatru29)";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];

    environment = {
      PORT = toString groupsRelayPort;
      DOMAIN = groupsDomain;
      RELAY_NAME = "NIP-29 Groups Relay";
      RELAY_DESCRIPTION = "A relay for NIP-29 group chats";
      DATABASE_PATH = "/var/lib/nostr-groups-relay/db";
    };

    script = ''
      export RELAY_PRIVKEY="$(cat "''${CREDENTIALS_DIRECTORY}/privkey")"
      exec ${groups-relay}/bin/groups-relay
    '';

    serviceConfig = {
      Type = "simple";
      LoadCredential = "privkey:${config.clan.core.vars.generators.nostr-groups-relay.files.privkey.path}";
      Restart = "on-failure";
      RestartSec = 5;

      DynamicUser = true;
      StateDirectory = "nostr-groups-relay";

      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      PrivateDevices = true;
      ReadWritePaths = [ "/var/lib/nostr-groups-relay" ];
    };
  };

  services.nginx.virtualHosts.${groupsDomain} = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://127.0.0.1:${toString groupsRelayPort};
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Proto $scheme;
      proxy_http_version 1.1;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection $connection_upgrade;
    '';
  };

  clan.core.vars.generators.nostr-groups-relay = {
    files.privkey = { };
    runtimeInputs = with pkgs; [ openssl ];
    script = ''
      openssl rand -hex 32 > "$out/privkey"
    '';
  };
}
