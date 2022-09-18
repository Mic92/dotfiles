{pkgs, modulesPath, lib, ...}: {
  nixpkgs.localSystem.system = "aarch64-linux";

  documentation.enable = false;

  imports = [
    "${modulesPath}/installer/sd-card/sd-image-aarch64.nix"
  ];

  boot.kernelParams = ["cma=32M"];

  boot.kernelPackages = lib.mkForce pkgs.linuxPackages;

  services.journald.extraConfig = ''
    Storage = volatile
    RuntimeMaxFileSize = 10M;
  '';

  swapDevices = [
    {
      device = "/swapfile";
      size = 1024;
    }
  ];

  hardware.enableRedistributableFirmware = true;
}
