{
  documentation.enable = false;
  hardware.emulab.enable = true;
  boot.loader.grub = {
    enable = true;
    version = 2;
    # what about nvme?
    device = "/dev/sda";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/NIXOS_BOOT";
    fsType = "ext4";
  };
  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXOS_ROOT";
    fsType = "ext4";
  };
}
