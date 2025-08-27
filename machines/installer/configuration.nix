{
  config,
  clan-core,
  self,
  ...
}:
{
  imports = [
    ./disko.nix
    ../../nixosModules/users.nix
    clan-core.nixosModules.installer
    clan-core.clanModules.trusted-nix-caches
    clan-core.clanModules.disk-id
  ];

  clan.core.deployment.requireExplicitUpdate = true;

  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.x86_64-linux;
  system.stateVersion = config.system.nixos.release;
}
