{ config, lib, pkgs, ... }:
{
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";
}
