{
  config,
  clan-core,
  inputs,
  ...
}:
{
  imports = [
    ./disko.nix
    clan-core.nixosModules.installer
    clan-core.clanModules.trusted-nix-caches
    clan-core.clanModules.disk-id
    clan-core.clanModules.iwd
  ];

  nixpkgs.pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
  system.stateVersion = config.system.nixos.version;
}
