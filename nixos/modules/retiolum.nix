{ config, pkgs, lib, ... }:

with lib;

let
  netname = "retiolum";
  cfg = config.networking.retiolum;

in {
  options = {
    networking.retiolum.ipv4 = mkOption {
      type = types.str;
      description = ''
        own ipv4 address
      '';
    };
    networking.retiolum.ipv6 = mkOption {
      type = types.str;
      description = ''
        own ipv6 address
      '';
    };
    networking.retiolum.nodename = mkOption {
      type = types.str;
      default = config.networking.hostName;
      description = ''
        tinc network name
      '';
    };
  };

  config = {
    services.tinc.networks.${netname} = {
      name = cfg.nodename;
      extraConfig = ''
        LocalDiscovery = yes

        ConnectTo = gum
        ConnectTo = ni
        ConnectTo = prism
        ConnectTo = eve
        AutoConnect = yes
      '';
    };

    networking.extraHosts = builtins.readFile (builtins.fetchurl {
      url = "https://retiolum.thalheim.io/etc.hosts";
    });

    environment.systemPackages = [ config.services.tinc.networks.${netname}.package ];

    systemd.services."tinc.${netname}" = {
      path = with pkgs; [ curl gnutar bzip2 ];
      preStart = ''
        (
          set -ex -o pipefail
          curl https://retiolum.thalheim.io/tinc-hosts.tar.bz2 -o /run/tinc-hosts.tar.bz2 &&
          rm -rf /etc/tinc/${netname}/hosts.tmp &&
          mkdir /etc/tinc/${netname}/hosts.tmp &&
          tar --strip-components 1 -xjf /run/tinc-hosts.tar.bz2 -C /etc/tinc/${netname}/hosts.tmp &&
          rm -rf /etc/tinc/${netname}/hosts /run/tinc-hosts.tar.bz2 &&
          mv /etc/tinc/${netname}/hosts{.tmp,}
        ) || true
      '';
    };

    networking.firewall.allowedTCPPorts = [ 655 ];
    networking.firewall.allowedUDPPorts = [ 655 ];
    #services.netdata.portcheck.checks.tinc.port = 655;

    systemd.network.enable = true;
    systemd.network.networks = {
      "${netname}".extraConfig = ''
        [Match]
        Name = tinc.${netname}

        [Network]
        Address=${cfg.ipv4}/12
        Address=${cfg.ipv6}/16
      '';
    };
  };
}
