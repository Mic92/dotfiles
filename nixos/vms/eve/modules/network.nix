{ config, lib, ... }:
with lib;

let
  cfg = config.networking.eve;
in {
  options = {
    networking.eve.ipv4.address = mkOption {
      type = types.str;
      default = "95.216.112.61";
    };
    networking.eve.ipv4.subnet = mkOption {
      type = types.str;
      default = "26";
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
      default = "64";
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
        Address = ${cfg.ipv4.address}/${cfg.ipv4.subnet}
        Gateway = ${cfg.ipv4.gateway}
        Address = ${cfg.ipv6.address}/${cfg.ipv6.subnet}
        Gateway = ${cfg.ipv6.gateway}
        IPv6AcceptRA = no
        IPForward = yes

        [DHCP]
        UseDNS = no
      '';
    };

    networking.nat = {
      enable = true;
      externalInterface = "eth0";
      internalIPs = [ "192.168.92.0/24" ];
    };

    # Hack so that network is considered up by boot.initrd.network and postCommands gets executed.
    boot.kernelParams = [ "ip=127.0.0.1:::::lo:none" ];

    boot.initrd.network = {
      enable = true;
      ssh = {
        enable = true;
        port = 2222;
        hostECDSAKey = "/run/keys/initrd-ssh-key";
      };
      postCommands = ''
        echo "zfs load-key -a && killall zfs" >> /root/.profile

        set -x

        ip link set dev eth0 up
        ip addr add ${cfg.ipv4.address}/${cfg.ipv4.subnet} dev eth0
        ip route add default via ${cfg.ipv4.gateway} dev eth0
        ip addr add ${cfg.ipv6.address}/${cfg.ipv6.subnet} dev eth0
        ip route add default via ${cfg.ipv6.gateway} dev eth0

        mkdir -p /mnt
        mount /dev/sda2 /mnt
        ip addr > /mnt/log
      '';
    };
  };
}
