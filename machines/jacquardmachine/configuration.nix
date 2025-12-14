{
  self,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    self.nixosModules.default
    # Framework 16 AMD AI 300 series with NVIDIA dGPU module
    self.inputs.nixos-hardware.nixosModules.framework-16-amd-ai-300-series-nvidia
    self.inputs.nix-index-database.nixosModules.nix-index
    { programs.nix-index-database.comma.enable = true; }
    self.inputs.disko.nixosModules.disko
    self.inputs.srvos.nixosModules.desktop

    ../../nixosModules/workstation.nix
    ../../nixosModules/shannan.nix
  ];

  environment.systemPackages = with pkgs; [ bottles ];

  # Disable envfs to fix systemd refusing to run with unpopulated /usr/
  services.envfs.enable = lib.mkForce false;

  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.x86_64-linux;

  # Pin to kernel 6.12 due to NVIDIA driver incompatibility with 6.18
  # The nvidia-open driver 580.105.08 fails to build on 6.18 with:
  # nvidia-uvm/uvm_va_range_device_p2p.c:363:13: error: too many arguments to function 'get_dev_pagemap'
  # See: https://github.com/NVIDIA/open-gpu-kernel-modules/issues/692
  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_6_12;

  networking.hostName = "jacquardmachine";

  system.stateVersion = "24.11";
}
