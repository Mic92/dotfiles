## Partitioning
# SSD1: 1MB Grub, 2G ext4/kernel + nixos rescue kexec, rest zfs
# $ sgdisk  -n 1:0:+1M -n 2:0:+2G -N 3 -t 1:ef02 -t 2:8304 -t 3:8304 /dev/sda
# SSD2: 1MB Grub, rest zfs
# $ sgdisk  -n 1:0:+1M -N 2 -t 1:ef02 -t 2:8304 /dev/sda
# $ nix-build '<nixpkgs/nixos>' -A config.system.build.kexec_bundle -I nixos-config=../kexec.nix -I nixcfg=kexec.nix
# $ mkfs.vfat -b32 -n bios-boot /dev/sda1
# $ mkfs.vfat -b32 -n bios-boot /dev/sdb1
# $ mkfs.ext4 -L linux-boot /dev/sda2
# $ mount /dev/sda2 /mnt
## Start nixos installer via kexec
# $ rsync -e ssh -a result root@95.216.112.61:/mnt
# rescue> /mnt/result
## Create encrypted zfs
# kexec> zpool create -o ashift=12 -o altroot=/mnt -O acltype=posixacl -O xattr=sa -O compression=lz4 zroot /dev/sda3
# kexec> zfs create -o encryption=aes-256-gcm -o keyformat=passphrase -o mountpoint=none zroot/root
# kexec> zfs create -o mountpoint=legacy -o sync=disabled zroot/root/tmp
# kexec> zfs create -o mountpoint=legacy -o com.sun:auto-snapshot=true zroot/root/home
# kexec> zfs create -o mountpoint=legacy -o com.sun:auto-snapshot=true zroot/root/nixos
# kexec> mount -t zfs zroot/root/nixos /mnt
# kexec> mkdir /mnt/{home,tmp,boot}
# kexec> mount /dev/sda2 /mnt/boot/
# kexec> mount -t zfs zroot/root/home /mnt/home/
# kexec> mount -t zfs zroot/root/tmp /mnt/tmp/
# continue with hetzner-bootstrap.nix
{ lib, ... }: {
  imports = [ ./base.nix ];

  boot.initrd.network.enable = lib.mkForce false;
}
## booting nixos from the rescue system
# rescue> mount /dev/sda2 /mnt; tar -C / -xf /mnt/nixos-system-x86_64-linux.tar.xz; /kexec_nixos
# kexec> zpool import zroot; zfs load-key -a; mount -t zfs zroot/root/nixos /mnt; mount /dev/sda2 /mnt/boot/; mount -t zfs zroot/root/home /mnt/home/; mount -t zfs zroot/root/tmp /mnt/tmp/
# kexec> nixos-enter # will mount

