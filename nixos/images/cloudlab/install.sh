#!/usr/bin/env nix-shell
#!nix-shell -p e2fsprogs -p bash -p jq -p util-linux -p coreutils -i bash

set -eux -o pipefail

flake=$1

rootDevice=
for dev in /dev/sd* /dev/nvme*; do
  if [[ $(sfdisk --dump --json $dev | jq '.partitiontable.partitions[0].bootable') == true ]]; then
    rootDevice=$dev
    break
  fi
done

if [[ -z $rootDevice ]]; then
  echo "no bootable partition found"
fi

sfdisk "$rootDevice" <<EOF
label: mbr
device: $rootDevice
unit: sectors
1 : start=2048, type=L, bootable
EOF

if [[ "$rootDevice" == /dev/nvme* ]]; then
  x=p
else
  x=
fi

fdisk -l
mkfs.ext4 "$rootDevice${x}1" -L NIXOS_ROOT

echo "wait for disk to appear"
while [[ ! -e /dev/disk/by-label/NIXOS_ROOT ]]; do
  sleep 1
done

mkdir -p /mnt
mount /dev/disk/by-label/NIXOS_ROOT /mnt

nix shell "nixpkgs#git" -c nixos-install --no-root-passwd --flake "$flake"
nixos-enter -c 'p=$(readlink -f /nix/var/nix/profiles/system); kexec --load $p/kernel --initrd $p/initrd --append="$(cat $p/kernel-params) init=$p/init)'
kexec -e
