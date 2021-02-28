# see hetzner/kexec.nix for example
{ lib, ... }:
let
  nixos-generators = builtins.fetchGit {
    url = https://github.com/krebs/nixos-generators;
    rev = "4edb2d6d74134223c55db1a2ad8c7c3618ca37fe"; #master on 2019-08-26
  };
in
{
  imports = [
    "${nixos-generators}/lib/kexec.nix"
    ./base-config.nix
  ];
}
