{ lib, pkgs, ... }: {
  boot.kernelPackages = lib.mkForce pkgs.linuxKernel.packages.linux_testing;

  # checkout the example folder for how to configure different disko layouts
  disko.devices = {
    disk.sda = {
      device = "/dev/sda";
      type = "disk";
      content = {
        type = "table";
        format = "gpt";
        partitions = [
          {
            name = "ESP";
            start = "1MiB";
            end = "500MiB";
            bootable = true;
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
            };
          }
          {
            name = "root";
            start = "500MiB";
            end = "100%";
            part-type = "primary";
            bootable = true;
            content = {
              type = "filesystem";
              format = "bcachefs";
              mountpoint = "/";
            };
          }
        ];
      };
    };
  };
}
