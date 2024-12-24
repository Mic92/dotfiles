{
  disko.devices = {
    disk = {
      root = {
        type = "disk";
        device = "/dev/nvme0n1";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              size = "1G";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "nofail" ];
              };
            };
            system = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "zroot";
              };
            };
            zil = {
              # for install test
              #size = "2G";
              size = "10G";
              content = {
                type = "zfs";
                pool = "zdata";
              };
            };
          };
        };
      };
      data = {
        type = "disk";
        device = "/dev/disk/by-id/wwn-0x5000c500af8b2a14";
        content = {
          type = "zfs";
          pool = "zdata";
        };
      };
    };
    zpool = {
      zroot = {
        type = "zpool";
        rootFsOptions = {
          mountpoint = "none";
          compression = "lz4";
          acltype = "posixacl";
          xattr = "sa";
          "com.sun:auto-snapshot" = "true";
        };
        options.ashift = "12";
        datasets = {
          "root" = {
            type = "zfs_fs";
            options.mountpoint = "none";
          };
          "root/nixos" = {
            type = "zfs_fs";
            options.mountpoint = "/";
            mountpoint = "/";
          };
          "root/home" = {
            type = "zfs_fs";
            options.mountpoint = "/home";
            mountpoint = "/home";
          };
          "root/tmp" = {
            type = "zfs_fs";
            mountpoint = "/tmp";
            options = {
              mountpoint = "/tmp";
              sync = "disabled";
            };
          };
        };
      };
      zdata = {
        type = "zpool";
        options.ashift = "12";
        rootFsOptions = {
          mountpoint = "none";
          compression = "lz4";
          acltype = "posixacl";
          xattr = "sa";
          "com.sun:auto-snapshot" = "true";
        };
        mode = {
          topology = {
            type = "topology";
            vdev = [
              {
                members = [ "/dev/disk/by-id/wwn-0x5000c500af8b2a14" ];
                # for tests
                #members = [ "/dev/vdb" ];
              }
            ];
            log = [
              {
                members = [ "/dev/disk/by-partlabel/disk-root-zil" ];
              }
            ];
          };
        };
        datasets = {
          "nas" = {
            type = "zfs_fs";
            mountpoint = "/mnt/hdd";
            mountOptions = [ "nofail" ];
          };
        };
      };
    };
  };
}
