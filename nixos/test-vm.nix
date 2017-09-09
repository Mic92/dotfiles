{ lib, config, pkgs, fetchgit, ... }:
# nixos-rebuild -I nixos-config=./test-vm.nix build-vm
{
  #boot.supportedFilesystems = [ "bcachefs" ];
  #boot.kernelPackages = pkgs.linuxPackages_testing_bcachefs;
  programs.zsh.ohMyZsh.enable = true;
  users.users.root.initialPassword = "root";
}
