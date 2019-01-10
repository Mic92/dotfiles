{ config, lib, ... }:
with lib;
{
  options = {
    networking.eve.ipv4.address = mkOption {
      type = types.str;
      default = "95.216.112.61";
    };
    networking.eve.ipv4.subnet = mkOption {
      type = types.str;
      default = "/26";
    };
    networking.eve.ipv4.gateway = mkOption {
      type = types.str;
      default = "95.216.112.1";
    };
    networking.eve.ipv6.address = mkOption {
      type = types.str;
      default = "2a01:4f9:2b:1605::1";
    };
    networking.eve.ipv6.subnet = mkOption {
      type = types.str;
      default = "/64";
    };
    networking.eve.ipv6.gateway = mkOption {
      type = types.str;
      default = "fe80::1";
    };
  };
  config = {
    networking.dhcpcd.enable = false;

    systemd.network = {
      enable = true;
      networks."eth0".extraConfig = ''
        [Match]
        Name = eth0

        [Network]
        Address = ${config.networking.eve.ipv4}/${config.networking.eve.ipv4.subnet}
        Gateway = ${config.networking.eve.ipv4.gateway}
        Address = ${config.networking.eve.ipv6}/${config.networking.eve.ipv6.subnet}
        Gateway = ${config.networking.eve.ipv6.gateway}
        IPv6AcceptRA = no
        IPForward = yes

        [DHCP]
        UseDNS = no
      '';
    };

    networking.firewall.enable = true;
    networking.nat = {
      enable = true;
      externalInterface = "eth0";
      internalIPs = [ "192.168.92.0/24" ];
    };

    initrd.network = {
      enable = true;
      ssh = {
        enable = true;
        port = 2222;
        hostECDSAKey = "/run/keys/initrd-ssh-key";
      };
      postCommands = ''
        echo "zfs load-key -a && killall zfs" >> /root/.profile

        ip addr add ${config.networking.eve.ipv4}/${config.networking.eve.ipv4.subnet} dev eth0
        ip route add default via ${config.networking.eve.ipv4.gateway}
        ip addr add ${config.networking.eve.ipv6.address}/${config.networking.eve.ipv6.subnet}
        ip route add default via ${config.networking.eve.ipv6.gateway} dev eth0
      '';
    };
  };
}
