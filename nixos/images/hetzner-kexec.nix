# SSD1: 1MB Grub, 2G ext4/kernel + nixos rescue kexec, rest zfs
# $ sgdisk  -n 1:0:+1M -n 2:0:+2G -N 3 -t 1:ef02 -t 2:8304 -t 3:8304 /dev/sda
# SSD2: zfs
# $ sgdisk  -n 1:0:+1M -n 2:0:+2G -N 3 -t 1:ef02 -t 2:8304 -t 3:8304 /dev/sda
# $ nix-build '<nixpkgs/nixos>' -A config.system.build.kexec_tarball -I nixos-config=./kexec.nix nixcfg=./hetzner-kexec.nix
# $ mkfs.vfat -b32 -n bios-boot /dev/sda1
# $ mkfs.ext4 -L linux-boot /dev/sda2
# $ mount /dev/sda2 /mnt
# $ rsync -e ssh -a result/tarball/nixos-system-x86_64-linux.tar.xz root@95.216.112.61:/mnt
# rescue> tar -C / -xf nixos-system-x86_64-linux.tar.xz
# rescue> /kexec_nixos
# $ zpool create -o ashift=12 -o altroot=/mnt -O acltype=posixacl -O xattr=sa -O compression=lz4 zroot /dev/sda3
# $ zfs create -o encryption=aes-256-gcm -o keyformat=passphrase -o mountpoint=none zroot/root

# $ zfs create -o mountpoint=legacy -o sync=disabled zroot/root/tmp
# $ zfs create -o mountpoint=legacy -o com.sun:auto-snapshot=true zroot/root/home
# $ zfs create -o mountpoint=legacy -o com.sun:auto-snapshot=true zroot/root/nixos
# $ mount -t zfs zroot/root/nixos /mnt
# $ mkdir /mnt/{home,tmp,boot}
# $ mount /dev/sda2 /mnt/boot/
# $ mount -t zfs zroot/root/home /mnt/home/
# $ mount -t zfs zroot/root/tmp /mnt/tmp/
# $ nixos-generate-config

# continue with hetzner-bootstrap.nix
{...}: {

  imports = [
    ./hetzner-base.nix
  ];
}
