{
  disko.devices = {
    disk.nvme = {
      device = "/dev/disk/by-id/nvme-TWSC_TSC3AN128-F2T70S_TTSFA25BAX11062";
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            type = "EF00";
            size = "1G";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [ "nofail" ];
            };
          };
          zfs = {
            size = "100%";
            content = {
              type = "zfs";
              pool = "zroot";
            };
          };
        };
      };
    };
    zpool.zroot = {
      type = "zpool";
      rootFsOptions = {
        compression = "lz4";
        acltype = "posixacl";
        xattr = "sa";
        "com.sun:auto-snapshot" = "true";
        mountpoint = "none";
      };
      options.ashift = "12";
      datasets = {
        "root" = {
          type = "zfs_fs";
          options.mountpoint = "/";
          mountpoint = "/";
        };
        "home" = {
          type = "zfs_fs";
          options.mountpoint = "/home";
          mountpoint = "/home";
        };
        "tmp" = {
          type = "zfs_fs";
          mountpoint = "/tmp";
          options = {
            mountpoint = "/tmp";
            sync = "disabled";
          };
        };
      };
    };
  };
}
