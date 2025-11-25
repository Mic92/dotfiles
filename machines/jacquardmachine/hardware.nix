{ lib, pkgs, ... }:
{
  # Hybrid Graphics: AMD (iGPU) + NVIDIA (dGPU)
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  boot.initrd.kernelModules = [ "amdgpu" ];

  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {
    # Use open-source kernel modules
    open = true;
    nvidiaSettings = true;

    # Hybrid graphics with PRIME offload for better battery life
    prime = {
      amdgpuBusId = "PCI:194:0:0"; # c2:00.0 in lspci (AMD iGPU)
      nvidiaBusId = "PCI:193:0:0"; # c1:00.0 in lspci (NVIDIA dGPU)

      # Offload mode: AMD iGPU by default, NVIDIA when needed
      offload = {
        enable = true;
        enableOffloadCmd = true;
      };
    };

    # Power management
    powerManagement.enable = true;
  };

  # Ensure recent kernel for best hardware support
  # Ryzen AI 300 series needs kernel >= 6.12 for full support
  boot.kernelPackages = lib.mkIf (lib.versionOlder pkgs.linux.version "6.12") (
    lib.mkDefault pkgs.linuxPackages_latest
  );

  # Power management for hybrid graphics
  services.power-profiles-daemon.enable = lib.mkDefault true;

  # Bluetooth support
  hardware.bluetooth.enable = lib.mkDefault true;
  hardware.bluetooth.powerOnBoot = lib.mkDefault true;
}
