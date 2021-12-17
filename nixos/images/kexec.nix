# see hetzner/kexec.nix for example
{ lib, ... }:
let
in
{
  imports = [
    ./base-config.nix
  ];
  kexec.autoReboot = false;

  boot.kernelParams = [ "console=ttyS0,115200n8" "console=ttyAMA0,115200n8" "console=tty0" ];
}
