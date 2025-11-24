{
  self,
  ...
}:
{
  imports = [
    self.nixosModules.default
    self.inputs.nixos-hardware.nixosModules.framework-16-inch-common
    self.inputs.nix-index-database.nixosModules.nix-index
    { programs.nix-index-database.comma.enable = true; }
    self.inputs.disko.nixosModules.disko
    self.inputs.srvos.nixosModules.desktop

    ../../nixosModules/workstation.nix
  ];

  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.x86_64-linux;

  networking.hostName = "jacquardmachine";

  system.stateVersion = "24.11";
}
