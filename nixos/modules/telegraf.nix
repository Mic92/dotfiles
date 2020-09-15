{ lib, config, ... }: {
  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 9273 ];
  services.telegraf = {
    enable = true;
    extraConfig = {
      inputs = {
        system = {};
        mem = {};
        file = {
          name_override = "ext4_errors";
          files = [ "/sys/fs/ext4/*/errors_count" ];
          data_format = "value";
        };
        zfs = lib.optionalAttrs (lib.any (fs: fs == "zfs") config.boot.supportedFilesystems) {};
        systemd_units = {};
        swap = {};
        disk.tagdrop = {
          fstype = [ "tmpfs" "ramfs" "devtmpfs" "devfs" "iso9660" "overlay" "aufs" "squashfs" ];
          device = [ "rpc_pipefs" "lxcfs" "nsfs" "borgfs" ];
        };
      };
      outputs = {
        prometheus_client = {
          listen = ":9273";
          metric_version = 2;
        };
      };
    };
  };
}
