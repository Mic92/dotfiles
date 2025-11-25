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

  networking.hostName = "jacquardmachine";

  system.stateVersion = "24.11";
}
