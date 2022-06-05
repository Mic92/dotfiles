{pkgs, modulesPath, ...}: {
  nixpkgs.localSystem.system = "aarch64-linux";

  documentation.enable = false;

  imports = [
    "${modulesPath}/installer/sd-card/sd-image-raspberrypi.nix"
  ];

  boot.kernelParams = ["cma=32M"];

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
