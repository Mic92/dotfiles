# build with:
# $ nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=./install-iso.nix
# $ dd status=progress bs=4M conv=fsync oflag=direct iflag=direct if=result/iso/nixos-*.iso of=/dev/sdb
# iso>
# iso> sgdisk -n 1:0:+1000M -N 2 -t 1:ef00 -t 2:8304 /dev/nvme0n1
# iso> mkfs.vfat -b32 /dev/nvme0n1p1 -N NIXOS_BOOT
# iso> ls -la /dev/disk/by-partuuid/
# iso> zpool create -f zroot /dev/disk/by-partuuid/046fdb0b-114f-4435-9d8c-957ac73b5cd2
# iso> mbuffer -6 -I 8023 | zfs receive -F zroot
# host> sudo zfs snapshot -r zroot@zfs-send
# host> sudo zfs send --raw -R zroot@zfs-send | mbuffer -6 -O [fd42:4492:6a6d:43:1::0]:8023
# iso> zfs load-key -a
# iso> mount -t zfs zroot/root/nixos /mnt
# iso> mount -t zfs zroot/root/home /mnt
# iso> mount /dev/nvme0n1p1 /mnt/boot
# iso> nix-shell -p git -p nix --run 'nixos-install --impure --flake /mnt/home/joerg/.homesick/repos/dotfiles#turingmachine'
{ modulesPath, ... }: {
  imports = [
    (modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix")
    # saves disk by including nixpkgs in the installer
    { system.installer.channel.enable = false; }
    ./base-config.nix
    ./zfs.nix
  ];
}
# legay cryptsetup:
# iso> cryptsetup luksFormat /dev/sda2
# iso> cryptsetup luksOpen /dev/sda2 root
# iso> zpool create -f zroot /dev/mapper/root
## New zroot setup
# iso> sgdisk -n 1:0:+1000M -N 2 -t 1:ef00 -t 2:8304 /dev/nvme0n1
# iso> mkfs.vfat -b32 /dev/nvme0n1p1 -N NIXOS_BOOT
# iso> ls -la /dev/disk/by-partuuid/
# iso> zpool create -O compression=lz4 -O dnodesize=auto -O normalization=formD -O atime=off -O xattr=sa -O relatime=on -O mountpoint=none -O acltype=posixacl / \
#   -f zroot /dev/disk/by-partuuid/046fdb0b-114f-4435-9d8c-957ac73b5cd2
# zfs create -o encryption=aes-256-gcm -o keyformat=passphrase -o mountpoint=none zroot/root
# zfs create -o mountpoint=/tmp -o setuid=off -o devices=off -o sync=disabled zroot/root/tmp
# zfs create -o mountpoint=/home -o com.sun:auto-snapshot=true zroot/root/home
# zfs create -o mountpoint=/ -o com.sun:auto-snapshot=true zroot/root/nixos
# zfs create -o mountpoint=none -o refreservation=1G zroot/reserved
# zfs create -o mountpoint=none zroot/docker
# mount -t zfs zroot/root/nixos /mnt
# mkdir /mnt/{home,tmp,boot}
# mount /dev/nvme0n1p1 /mnt/boot/
# mount -t zfs -o zfsutil zroot/root/home /mnt/home
# mount -t zfs -o zfsutil zroot/root/tmp /mnt/tmp/
# chmod 777 /mnt/tmp
# nixos-generate-config  --root /mnt
# nixos-install
