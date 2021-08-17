# see hetzner/kexec.nix for example
{ lib, ... }:
let
  nixos-generators = builtins.fetchGit {
    url = "https://github.com/krebs/nixos-generators";
    rev = "022ef440af8dc237ab1f59fa363cb1e25783ec3e"; #master on 29.06.2021
  };
in
{
  imports = [
    "${nixos-generators}/formats/kexec-bundle.nix"
    "${nixos-generators}/format-module.nix"
    ./base-config.nix
  ];

  boot.kernelParams = [ "console=ttyS0,115200n8" "console=ttyAMA0,115200n8" "console=tty0" ];
}
