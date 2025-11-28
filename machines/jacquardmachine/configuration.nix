{
  self,
  lib,
  ...
}:
{
  imports = [
    self.nixosModules.default
    # Framework 16 common module (not exposed as flake output, imported by path)
    "${self.inputs.nixos-hardware}/framework/16-inch/common"
    ./hardware.nix
    self.inputs.nix-index-database.nixosModules.nix-index
    { programs.nix-index-database.comma.enable = true; }
    self.inputs.disko.nixosModules.disko
    self.inputs.srvos.nixosModules.desktop

    ../../nixosModules/workstation.nix
    ../../nixosModules/shannan.nix
  ];

  # Disable envfs to fix systemd refusing to run with unpopulated /usr/
  services.envfs.enable = lib.mkForce false;

  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.x86_64-linux;

  networking.hostName = "jacquardmachine";

  system.stateVersion = "24.11";
}
