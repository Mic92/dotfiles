{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.networking.eve;
in {
  options = {
    networking.eve.ipv4.address = mkOption {
      type = types.str;
      default = "88.99.244.96";
    };
    networking.eve.ipv4.cidr = mkOption {
      type = types.str;
      default = "26";
    };

    networking.eve.ipv4.gateway = mkOption {
      type = types.str;
      default = "88.99.244.65";
    };

    networking.eve.ipv6.addresses = mkOption {
      type = types.listOf types.str;
      default = [
        "2a01:4f8:10b:49f::1"
        # ssh on port 443
        "2a01:4f8:10b:49f::2"
        # xmpp on port 443
        "2a01:4f8:10b:49f::3"
        # tinc on port 443
        "2a01:4f8:10b:49f::4"
        # kresd on port 443
        "2a01:4f8:10b:49f::5"
      ];
    };

    networking.eve.ipv6.subnet = mkOption {
      type = types.str;
      default = "2a01:4f9:2b:1605::/64";
    };

    networking.eve.ipv6.cidr = mkOption {
      type = types.str;
      default = "64";
    };
    networking.eve.ipv6.gateway = mkOption {
      type = types.str;
      default = "fe80::1";
    };
  };
  config = {
    networking.dhcpcd.enable = false;
    services.resolved.enable = false;
    networking.nameservers = ["127.0.0.1"];
    networking.usePredictableInterfaceNames = false;

    systemd.network = {
      enable = true;
      networks."eth0".extraConfig = ''
        [Match]
        Name = e*

        [Network]
        DHCP = ipv4

        ${
          concatMapStringsSep "\n" (address: ''
            Address = ${address}/${cfg.ipv6.cidr}
          '')
          cfg.ipv6.addresses
        }
        Gateway = ${cfg.ipv6.gateway}
        IPv6AcceptRA = no
        IPForward = yes

        [DHCP]
        UseDNS = no
      '';
    };

    # Hack so that network is considered up by boot.initrd.network and postCommands gets executed.
    boot.kernelParams = ["ip=127.0.0.1:::::lo:none"];

    sops.secrets.initrd-ssh-key = {};
    boot.initrd.network = {
      enable = true;
      ssh = {
        enable = true;
        port = 2222;
        hostKeys = [
          #config.sops.secrets.initrd-ssh-key.path
          "/var/lib/initrd-ssh-key"
        ];
      };
      postCommands = ''
        ip link set dev eth0 up

        ip addr add ${cfg.ipv4.address}/${cfg.ipv4.cidr} dev eth0
        ip route add ${cfg.ipv4.gateway} dev eth0
        ip route add default via ${cfg.ipv4.gateway} dev eth0

        ${
          concatMapStringsSep "\n"
          (address: ''
            ip -6 addr add ${address}/${cfg.ipv6.cidr} dev eth0
          '')
          cfg.ipv6.addresses
        }
        ip -6 route add ${cfg.ipv6.gateway} dev eth0
        ip -6 route add default via ${cfg.ipv6.gateway} dev eth0
      '';
    };
    boot.initrd.kernelModules = ["e1000e"];

    # often hangs
    systemd.services.systemd-networkd-wait-online.enable = lib.mkForce false;
  };
}
