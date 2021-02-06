{ config, lib, pkgs, ... }:
{
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.shannan = { ... }: {
    services.syncthing.enable = true;
  };
}
