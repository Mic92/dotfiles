# build with:
# $ nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=/etc/nixos/install-iso.nix
# $ dd if=result/iso/nixos-*.iso of=/dev/sdb
# iso>
# iso> nc -6 -w 120 -l -p 8023 | zfs receive -F zroot
# host> sudo zfs send -R zroot@zfs-send | nc -w 20 fe80::6af7:28ff:feb2:8706%enp0s25
{ config, lib, pkgs, modulesPath, ... }:
{
   imports = [
     <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
   ];

   boot = {
     kernelParams = [ "copytoram=1" ];
     supportedFilesystems = [ "zfs" ];
   };

   networking.firewall.enable = false;
   services.openssh = {
     enable = true;
     startWhenNeeded = true;
   };
   users.extraUsers.root.openssh.authorizedKeys.keys = [
     "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine"
   ];
}
