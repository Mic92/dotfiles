{
  fileSystems."/boot" = {
    fsType = "ext4";
    label = "boot";
  };
  fileSystems."/" = {
    fsType = "ext4";
    label = "root-ssd";
  };

  fileSystems."/zdata" = {
    device = "zdata";
    fsType = "zfs";
    options = [ "zfsutil" "nofail" ];
  };

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  boot.kernelParams = [ "boot.shell_on_fail" ];
  srvos.boot.consoles = [ "ttyS2,115200" ];

  boot.initrd.availableKernelModules = [
    "sd_mod"
    "xhci_pci"
    "usb_storage"
    "uas"
    "ahci"
    "usbhid"
    "phy_rockchip_pcie"
    "pcie_rockchip_host"
  ];
}
