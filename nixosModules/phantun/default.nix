# Phantun NixOS module - UDP to fake TCP tunnel
# Server: listens on TCP and forwards to local UDP
# Client: connects to TCP server and exposes local UDP
{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  cfg = config.services.phantun;
  phantun = self.packages.${pkgs.system}.phantun;
in
{
  options.services.phantun = {
    server = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            enable = lib.mkEnableOption "phantun server instance";

            listenPort = lib.mkOption {
              type = lib.types.port;
              description = "TCP port to listen on for fake TCP connections";
              example = 4567;
            };

            remoteUdp = lib.mkOption {
              type = lib.types.str;
              description = "Local UDP address:port to forward packets to";
              example = "127.0.0.1:51820";
            };

            tun = lib.mkOption {
              type = lib.types.str;
              default = "tun0";
              description = "TUN interface name";
            };

            tunLocalAddress = lib.mkOption {
              type = lib.types.str;
              default = "192.168.201.1";
              description = "Local IPv4 address for TUN interface";
            };

            tunPeerAddress = lib.mkOption {
              type = lib.types.str;
              default = "192.168.201.2";
              description = "Peer IPv4 address for TUN interface";
            };

            tunLocalAddress6 = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              example = "fcc9::1";
              description = "Local IPv6 address for TUN interface";
            };

            tunPeerAddress6 = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              example = "fcc9::2";
              description = "Peer IPv6 address for TUN interface";
            };

            interface = lib.mkOption {
              type = lib.types.str;
              description = "Network interface for incoming connections";
              example = "eth0";
            };
          };
        }
      );
      default = { };
      description = "Phantun server instances";
    };

    client = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            enable = lib.mkEnableOption "phantun client instance";

            localUdp = lib.mkOption {
              type = lib.types.str;
              description = "Local UDP address:port to expose";
              example = "127.0.0.1:51820";
            };

            remoteAddress = lib.mkOption {
              type = lib.types.str;
              description = "Remote phantun server address:port";
              example = "example.com:4567";
            };

            tun = lib.mkOption {
              type = lib.types.str;
              default = "tun0";
              description = "TUN interface name";
            };

            tunLocalAddress = lib.mkOption {
              type = lib.types.str;
              default = "192.168.200.1";
              description = "Local IPv4 address for TUN interface";
            };

            tunPeerAddress = lib.mkOption {
              type = lib.types.str;
              default = "192.168.200.2";
              description = "Peer IPv4 address for TUN interface";
            };

            tunLocalAddress6 = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              example = "fcc8::1";
              description = "Local IPv6 address for TUN interface";
            };

            tunPeerAddress6 = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              example = "fcc8::2";
              description = "Peer IPv6 address for TUN interface";
            };

            interface = lib.mkOption {
              type = lib.types.str;
              description = "Network interface for outgoing connections";
              example = "eth0";
            };
          };
        }
      );
      default = { };
      description = "Phantun client instances";
    };
  };

  config = lib.mkMerge [
    # Server instances
    (lib.mkIf (cfg.server != { }) {
      boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

      systemd.services = lib.mapAttrs' (
        name: serverCfg:
        lib.nameValuePair "phantun-server-${name}" {
          description = "Phantun Server (${name})";
          after = [ "network.target" ];
          wantedBy = [ "multi-user.target" ];

          serviceConfig = {
            Type = "simple";
            ExecStart =
              let
                args = [
                  "--local"
                  (toString serverCfg.listenPort)
                  "--remote"
                  serverCfg.remoteUdp
                  "--tun"
                  serverCfg.tun
                  "--tun-local"
                  serverCfg.tunLocalAddress
                  "--tun-peer"
                  serverCfg.tunPeerAddress
                ]
                ++ lib.optionals (serverCfg.tunLocalAddress6 != null) [
                  "--tun-local6"
                  serverCfg.tunLocalAddress6
                ]
                ++ lib.optionals (serverCfg.tunPeerAddress6 != null) [
                  "--tun-peer6"
                  serverCfg.tunPeerAddress6
                ];
              in
              "${phantun}/bin/server ${lib.escapeShellArgs args}";
            Restart = "on-failure";
            RestartSec = 5;

            # Security hardening
            AmbientCapabilities = [ "CAP_NET_ADMIN" ];
            CapabilityBoundingSet = [ "CAP_NET_ADMIN" ];
            DynamicUser = true;
            PrivateTmp = true;
            ProtectSystem = "strict";
            ProtectHome = true;
            NoNewPrivileges = true;
          };

          environment.RUST_LOG = "info";
        }
      ) (lib.filterAttrs (_: v: v.enable) cfg.server);

      # Firewall rules for server - DNAT incoming TCP to TUN
      networking.nftables.tables.phantun-server = lib.mkIf config.networking.nftables.enable {
        family = "inet";
        content = lib.concatMapStrings (
          name:
          let
            serverCfg = cfg.server.${name};
          in
          ''
            chain prerouting-${name} {
              type nat hook prerouting priority dstnat; policy accept;
              iifname "${serverCfg.interface}" tcp dport ${toString serverCfg.listenPort} dnat ip to ${serverCfg.tunPeerAddress}
              ${lib.optionalString (serverCfg.tunPeerAddress6 != null) ''
                iifname "${serverCfg.interface}" tcp dport ${toString serverCfg.listenPort} dnat ip6 to ${serverCfg.tunPeerAddress6}
              ''}
            }
          ''
        ) (builtins.attrNames (lib.filterAttrs (_: v: v.enable) cfg.server));
      };

      # Open firewall ports for servers
      networking.firewall.allowedTCPPorts = lib.mapAttrsToList (_: v: v.listenPort) (
        lib.filterAttrs (_: v: v.enable) cfg.server
      );
    })

    # Client instances
    (lib.mkIf (cfg.client != { }) {
      boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

      systemd.services = lib.mapAttrs' (
        name: clientCfg:
        lib.nameValuePair "phantun-client-${name}" {
          description = "Phantun Client (${name})";
          after = [ "network.target" ];
          wantedBy = [ "multi-user.target" ];

          serviceConfig = {
            Type = "simple";
            ExecStart =
              let
                args = [
                  "--local"
                  clientCfg.localUdp
                  "--remote"
                  clientCfg.remoteAddress
                  "--tun"
                  clientCfg.tun
                  "--tun-local"
                  clientCfg.tunLocalAddress
                  "--tun-peer"
                  clientCfg.tunPeerAddress
                ]
                ++ lib.optionals (clientCfg.tunLocalAddress6 != null) [
                  "--tun-local6"
                  clientCfg.tunLocalAddress6
                ]
                ++ lib.optionals (clientCfg.tunPeerAddress6 != null) [
                  "--tun-peer6"
                  clientCfg.tunPeerAddress6
                ];
              in
              "${phantun}/bin/client ${lib.escapeShellArgs args}";
            Restart = "on-failure";
            RestartSec = 5;

            # Security hardening
            AmbientCapabilities = [ "CAP_NET_ADMIN" ];
            CapabilityBoundingSet = [ "CAP_NET_ADMIN" ];
            DynamicUser = true;
            PrivateTmp = true;
            ProtectSystem = "strict";
            ProtectHome = true;
            NoNewPrivileges = true;
          };

          environment.RUST_LOG = "info";
        }
      ) (lib.filterAttrs (_: v: v.enable) cfg.client);

      # Firewall rules for client - MASQUERADE outgoing from TUN
      networking.nftables.tables.phantun-client = lib.mkIf config.networking.nftables.enable {
        family = "inet";
        content = lib.concatMapStrings (
          name:
          let
            clientCfg = cfg.client.${name};
          in
          ''
            chain postrouting-${name} {
              type nat hook postrouting priority srcnat; policy accept;
              iifname "${clientCfg.tun}" oifname "${clientCfg.interface}" masquerade
            }
          ''
        ) (builtins.attrNames (lib.filterAttrs (_: v: v.enable) cfg.client));
      };
    })
  ];
}
