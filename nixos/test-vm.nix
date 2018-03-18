{ lib, config, pkgs, fetchgit, ... }:
# export QEMU_OPTS="-nographic -serial mon:stdio" QEMU_KERNEL_PARAMS=console=ttyS0 && echo "stty rows $LINES columns $COLUMNS" && nixos-rebuild -I nixos-config=./test-vm.nix build-vm
{
  users.users.root.initialPassword = "root";
  systemd.services."serial-getty@ttyS0".enable = true;
  networking.firewall.enable = false;
}
