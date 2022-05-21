{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./nspawn-network.nix
    ./joe01.nix
    ./joe02.nix
  ];
}
