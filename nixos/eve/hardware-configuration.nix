{ lib
, modulesPath
, ...
}: {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
  ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "sd_mod"
    "nvme"
  ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
