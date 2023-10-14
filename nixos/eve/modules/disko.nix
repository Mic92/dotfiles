let
  mirrorBoot = idx: {
    type = "disk";
    device = "/dev/nvme${idx}n1";
    content = {
      type = "gpt";
      partitions = {
        ESP = {
          size = "1G";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot${idx}";
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
in
{
  boot.initrd.postDeviceCommands = ''
    while ! test -f /tmp/decrypted; do
      echo "wait for zfs to be decrypted"
      sleep 1
    done
  '';

  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    efiInstallAsRemovable = true;
    mirroredBoots = [
      { path = "/boot0"; devices = [ "nodev" ]; }
      { path = "/boot1"; devices = [ "nodev" ]; }
    ];
  };

  disko.devices = {
    disk = {
      x = mirrorBoot "0";
      y = mirrorBoot "1";
    };
    zpool = {
      zroot = {
        type = "zpool";
        mode = "mirror";
        rootFsOptions = {
          compression = "lz4";
          "com.sun:auto-snapshot" = "true";
        };
        datasets = {
          "root" = {
            type = "zfs_fs";
            options = {
              mountpoint = "none";
              encryption = "aes-256-gcm";
              keyformat = "hex";
              #keylocation = "file:///tmp/secret.key";
              keylocation = "prompt";
            };
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
        };
      };
    };
  };
}
