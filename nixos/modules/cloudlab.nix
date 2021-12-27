{
  hardware.emulab.enable = true;
  documentation.enable = false;

  imports = [
    ./sshd.nix
  ];

  # bootloader loads bootloader from first partition not disk!
  boot.loader.grub = {
    device = "/dev/disk/by-label/NIXOS_ROOT";
    forceInstall = true;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXOS_ROOT";
    fsType = "ext4";
  };
}
