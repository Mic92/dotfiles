{ config, lib, pkgs, ... }:
{
  imports = [
    ./nspawn-network.nix
    # TODO monitoring -> email: j03@c3d2.de
    ./joe01.nix
  ];
}
