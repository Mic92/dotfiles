#!/usr/bin/env bash

set -eux -o pipefail

rootDevice=$1

isEFI() {
  [ -d /sys/firmware/efi ]
}

if isEFI; then
 biosPartition=""
else
 biosPartition="3 : size=4096, type=21686148-6449-6E6F-744E-656564454649"
fi

sfdisk "$rootDevice" <<EOF
label: gpt
device: $rootDevice
unit: sectors
1 : size=$((2048 * 256)), type=0FC63DAF-8483-4772-8E79-3D69D8477DE4
${biosPartition}
2 : type=0FC63DAF-8483-4772-8E79-3D69D8477DE4
EOF

mkdir -p /mnt

if [[ "$rootDevice" == /dev/nvme* ]]; then
  x=p
else
  x=
fi

fdisk -l
mkfs.vfat "$rootDevice${x}1" -n NIXOS_BOOT
mkfs.ext4 "$rootDevice${x}2" -L NIXOS_ROOT

echo "wait for disk to appear"
while [[ ! -e /dev/disk/by-label/NIXOS_BOOT ]] || [[ ! -e /dev/disk/by-label/NIXOS_ROOT ]]; do
  sleep 1
done

mkdir -p /mnt
mount /dev/disk/by-label/NIXOS_ROOT /mnt
mkdir -p /mnt/boot
mount /dev/disk/by-label/NIXOS_BOOT /mnt/boot

#nix shell "nixpkgs#git" -- nixos-install --no-root-passwd --flake /etc/nixos
