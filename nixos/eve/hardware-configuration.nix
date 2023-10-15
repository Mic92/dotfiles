{ lib
, modulesPath
, ...
}: {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
  ];

  boot = {
    zfs.enableUnstable = true;
    zfs.requestEncryptionCredentials = true;

    initrd.availableKernelModules = [
      "xhci_pci"
      "ahci"
      "sd_mod"
      "nvme"
    ];
  };

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
