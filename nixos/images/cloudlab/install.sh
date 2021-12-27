#!/usr/bin/env bash

set -eux -o pipefail

rootDevice=$1

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

install -m700 -D /etc/ssh/ssh_host_ed25519_key /mnt/etc/ssh/ssh_host_ed25519_key
install -m700 -D /etc/ssh/ssh_host_rsa_key /mnt/etc/ssh/ssh_host_rsa_key
nix shell "nixpkgs#git" -c nixos-install --no-root-passwd --flake '/etc/nixos#cloudlab-node'
