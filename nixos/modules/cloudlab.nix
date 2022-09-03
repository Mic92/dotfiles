{
  hardware.emulab.enable = true;
  documentation.enable = false;

  systemd.services.miniond.serviceConfig.Restart = "on-failure";
  systemd.services.miniond.serviceConfig.RestartSec = 2;

  imports = [
    ./sshd
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
