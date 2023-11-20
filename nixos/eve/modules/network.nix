{ config
, lib
, ...
}:
{
  options = {
    networking.eve.ipv4.address = lib.mkOption {
      type = lib.types.str;
      default = "95.217.199.121";

    };
    networking.eve.ipv4.cidr = lib.mkOption {
      type = lib.types.str;
      default = "26";
    };

    networking.eve.ipv4.gateway = lib.mkOption {
      type = lib.types.str;
      default = "95.217.199.65";
    };

    networking.eve.ipv6.address = lib.mkOption {
      type = lib.types.str;
      default = "2a01:4f9:4a:42e8::1";
    };

    networking.eve.ipv6.subnet = lib.mkOption {
      type = lib.types.str;
      default = "2a01:4f9:4a:42e8::/64";
    };

    networking.eve.ipv6.cidr = lib.mkOption {
      type = lib.types.str;
      default = "64";
    };
    networking.eve.ipv6.gateway = lib.mkOption {
      type = lib.types.str;
      default = "fe80::1";
    };
  };
  config = {
    networking.dhcpcd.enable = false;
    networking.nameservers = [ "127.0.0.1" ];

    systemd.network.networks."10-uplink".networkConfig.Address = config.networking.eve.ipv6.address;

    boot.initrd.systemd.network.networks."10-uplink" = config.systemd.network.networks."10-uplink";

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
    };
    boot.initrd.kernelModules = [ "igb" ];
  };
}
