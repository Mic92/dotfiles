{
  config,
  clan-core,
  self,
  ...
}:
{
  imports = [
    ./disko.nix
    clan-core.nixosModules.installer
  ];

  clan.core.deployment.requireExplicitUpdate = true;

  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.x86_64-linux;
  system.stateVersion = config.system.nixos.release;
}
