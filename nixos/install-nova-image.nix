{ config, lib, pkgs, ... }:
# nix-build '<nixpkgs/nixos>' -A config.system.build.novaImage -I nixos-config=./install-nova-image.nix
with lib;
{
  imports =
    [ <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
      <nixpkgs/nixos/modules/virtualisation/nova-config.nix>
    ];
  system.build.novaImage = import <nixpkgs/nixos/lib/make-disk-image.nix> {
    inherit lib config;
    pkgs = import <nixpkgs> { inherit (pkgs) system; }; # ensure we use the regular qemu-kvm package
    diskSize = 8192;
    format = "qcow2";
    configFile = ./install-image.nix;
  };
}
