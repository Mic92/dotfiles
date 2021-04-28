{ pkgs ? import <nixpkgs> {},
  netboot ? pkgs.callPackage ./netboot.nix {}
}:

# 1. Enable nixos/modules/dnsmasq.nix
# $ iptables -F
# $ sudo $(nix-build ./netboot-pixie-core.nix)
pkgs.writeShellScript "netboot-pixie-core" ''
  set -euo pipefail
  init=$(${pkgs.gnugrep}/bin/grep -ohP 'init=\S+' ${netboot}/netboot.ipxe)
  ${pkgs.pixiecore}/bin/pixiecore \
    boot ${netboot}/bzImage ${netboot}/initrd \
    --cmdline "$init loglevel=4" \
    --debug --dhcp-no-bind --port 64172 --status-port 64172
''
