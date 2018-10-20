{ lib, ... }:

{
  imports = [ 
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
  ];

  boot = {
    zfs.enableUnstable = true;

    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
    };

    initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci" "sd_mod" "sr_mod" ];
    initrd.luks.devices = [ { name = "root"; device = "/dev/sda2"; } ];
  };

  fileSystems = [{
    mountPoint = "/boot";
    device = "/dev/disk/by-uuid/56FC-E60D";
    fsType = "vfat";
    options = ["nofail"];
  }] ++ (lib.mapAttrs (fs: mountPoint: {
    device = fs;
    mountPoint = mountPoint;
    fsType = "zfs";
    options = ["nofail"];
  }) {
    "zroot/root" = "/";
    "zroot/home" = "/home";
    "zroot/tmp" = "/tmp";
    "zroot/docker" = "/var/lib/docker";
    "zroot/data" = "/data";
    "zroot/data/backup" = "/data/backup";
    "zroot/data/backup/devkid" = "/data/backup/devkid";
  });
}
