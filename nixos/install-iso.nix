# build with:
# $ nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=/etc/nixos/install-iso.nix
# $ dd if=result/iso/nixos-*.iso of=/dev/sdb
# iso>
# iso> sgdisk -n 1:0:+300M -N 2 -t 1:ef02 -t 2:8300 /dev/sda
# iso> mkfs.vfat /dev/sda1
# iso> cryptsetup luksFormat /dev/sda2
# iso> cryptsetup luksOpen /dev/sda2 root
# iso> zpool create -f zroot /dev/mapper/root
# iso> nc -6 -w 120 -l -p 8023 | zfs receive -F zroot
# host> sudo zfs snapshot -r zroot@zfs-send
# host> sudo zfs send -R zroot@zfs-send | nc -w 20 fe80::6af7:28ff:feb2:8706%enp0s25 8023
{ config, lib, pkgs, modulesPath, ... }:
{
   imports = [
     <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
     ./install-image.nix
   ];
}
