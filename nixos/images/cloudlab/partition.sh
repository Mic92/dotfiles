#!/usr/bin/env nix-shell
#!nix-shell -p e2fsprogs -p bash -p jq -p util-linux -p coreutils -i bash
# shellcheck shell=bash

set -eux -o pipefail

rootDevice=
for dev in /dev/sd* /dev/nvme*; do
  if [[ $(sfdisk --dump --json "$dev" | jq '.partitiontable.partitions[0].bootable') == true ]]; then
    rootDevice=$dev
    break
  fi
done

if [[ -z $rootDevice ]]; then
  echo "no bootable partition found"
fi

if [[ ! -e /dev/disk/by-label/NIXOS_ROOT ]]; then
  sfdisk "$rootDevice" <<EOF
  label: mbr
  device: $rootDevice
  unit: sectors
  1 : start=2048, type=L, bootable
EOF

  if [[ $rootDevice == /dev/nvme* ]]; then
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
fi

mkdir -p /mnt
if ! mountpoint /mnt; then
  mount /dev/disk/by-label/NIXOS_ROOT /mnt
fi
