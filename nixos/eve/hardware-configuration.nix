{ lib, modulesPath, ... }:

{
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
  ];

  boot = {
    zfs.enableUnstable = true;
    zfs.requestEncryptionCredentials = true;

    loader.grub = {
      enable = true;
      version = 2;
      devices = [ "/dev/sda" "/dev/sdb" ];
    };

    initrd.availableKernelModules = [ "xhci_pci" "ahci" "sd_mod" ];
  };

  fileSystems."/" = {
    device = "zroot/root/nixos";
    fsType = "zfs";
  };

  fileSystems."/home" = {
    device = "zroot/root/home";
    fsType = "zfs";
    options = ["nofail"];
  };

  fileSystems."/tmp" = {
    device = "zroot/root/tmp";
    fsType = "zfs";
    options = ["nofail"];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/18630124-7e4f-406b-ae58-05c6dd73ada2";
    fsType = "ext4";
    options = ["nofail"];
  };

  fileSystems."/data" = {
    device = "data/root/data";
    fsType = "zfs";
    options = ["nofail"];
  };

  fileSystems."/data/backup" = {
    device = "data/root/backup";
    fsType = "zfs";
    options = ["nofail"];
  };

  fileSystems."/data/backup/devkid" = {
    device = "data/root/backup/devkid";
    fsType = "zfs";
    options = ["nofail"];
  };

  fileSystems."/var/lib/docker" = {
    device = "data/root/docker";
    fsType = "zfs";
    options = ["nofail"];
  };

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
