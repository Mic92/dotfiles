{ pkgs, ... }:
{
  nixpkgs.localSystem.system = "aarch64-linux";

  documentation.enable = false;

  boot.loader.grub.enable = false;
  boot.loader.raspberryPi.enable = true;
  boot.loader.raspberryPi.version = 3;
  boot.loader.raspberryPi.uboot.enable = true;

  # Kernel configuration
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.kernelParams = [ "cma=32M" ];

  services.journald.extraConfig = ''
    Storage = volatile
    RuntimeMaxFileSize = 10M;
  '';

  swapDevices = [
    { device = "/swapfile"; size = 1024; }
  ];

  hardware.enableRedistributableFirmware = true;

  fileSystems = {
    # not needed with uboot
    #"/boot/firmware" = {
    #  device = "/dev/disk/by-label/FIRMWARE";
    #  fsType = "vfat";
    #};
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };
}
