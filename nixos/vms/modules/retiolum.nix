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
        ConnectTo = gum
        ConnectTo = ni
        ConnectTo = prism
        ConnectTo = eve
        AutoConnect = yes
      '';
    };

    networking.extraHosts = builtins.readFile (pkgs.fetchurl {
      name = "retiolum.hosts";
      url = "https://lassul.us/retiolum.hosts";
      # FIXME
      sha256 = "0qi7bcmkhj2kmb3qrgs3q220q9z298awy91wm9nhk4vbgj7271g0";
    });

    environment.systemPackages = [ config.services.tinc.networks.${netname}.package ];

    systemd.services."tinc.${netname}" = {
      path = with pkgs; [ curl gnutar bzip2 ];
      preStart = ''
        curl https://lassul.us/retiolum-hosts.tar.bz2 | tar -xjvf - -C /etc/tinc/${netname}/ || true
      '';
    };

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
