{ config
, lib
, ...
}:
with lib; let
  cfg = config.networking.eve;
in
{
  options = {
    networking.eve.ipv4.address = mkOption {
      type = types.str;
      default = "95.217.199.121";

    };
    networking.eve.ipv4.cidr = mkOption {
      type = types.str;
      default = "26";
    };

    networking.eve.ipv4.gateway = mkOption {
      type = types.str;
      default = "95.217.199.65";
    };

    networking.eve.ipv6.address = mkOption {
      type = types.str;
      default = "2a01:4f9:4a:42e8::1";
    };

    networking.eve.ipv6.subnet = mkOption {
      type = types.str;
      default = "2a01:4f9:4a:42e8::/64";
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
    networking.nameservers = [ "127.0.0.1" ];

    # Hack so that network is considered up by boot.initrd.network and postCommands gets executed.
    boot.kernelParams = [ "ip=127.0.0.1:::::lo:none" ];

    systemd.network.networks."10-uplink".networkConfig.Address = config.networking.eve.ipv6.address;

    #boot.initrd.postDeviceCommands = ''
    #  while ! test -f /root/decrypted; do
    #    echo "wait for zfs to be decrypted"
    #    sleep 1
    #  done
    #'';
    boot.initrd.network = {
      enable = true;
      ssh = {
        enable = true;
        port = 2222;
        hostKeys = [
          #FIXME this has to be manually uploaded during installation...
          # scp /tmp/initrd-ssh-key root@95.217.199.121:/mnt/var/lib/initrd-ssh-key
          # nixos-enter
          # realpath /run/current-system
          # exit
          # nixos-install --no-root-passwd --no-channel-copy --root /mnt --system /nix/store/1j1cf7l6f2b3hfd2dxmkmrvg5kblhgkl-nixos-system-eve-23.11.20231014.da24e6f
          #config.sops.secrets.eve-initrd-ssh-key.path
          "/var/lib/initrd-ssh-key"
        ];
      };
      postCommands = ''
        ls -la
        while ! ip link show dev enp35s0; do
          echo "wait for enp35s0 to be available"
          ip a
          sleep 1
        done
        ip link set dev enp35s0 up

        ip addr add ${cfg.ipv4.address}/${cfg.ipv4.cidr} dev enp35s0
        ip route add ${cfg.ipv4.gateway} dev enp35s0
        ip route add default via ${cfg.ipv4.gateway} dev enp35s0

        ip -6 addr add ${cfg.ipv6.address}/${cfg.ipv6.cidr} dev enp35s0
        ip -6 route add ${cfg.ipv6.gateway} dev enp35s0
        ip -6 route add default via ${cfg.ipv6.gateway} dev enp35s0
      '';
    };
    boot.initrd.kernelModules = [ "igb" ];
  };
}
