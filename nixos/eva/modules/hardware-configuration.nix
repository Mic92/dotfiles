{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  #imports = [
  #  (modulesPath + "/profiles/qemu-guest.nix")
  #];

  #boot.loader.grub.devices = [ "/dev/sda" ];
  #boot.initrd.availableKernelModules = [ "ata_piix" "virtio_pci" "xhci_pci" "sd_mod" "sr_mod" ];

  #fileSystems."/" = {
  #  device = "/dev/disk/by-uuid/84053adc-49bc-4e02-8a19-3838bf3a43fd";
  #  fsType = "ext4";
  #};
  imports = [
    (modulesPath + "/virtualisation/lxc-container.nix")
  ];
}
