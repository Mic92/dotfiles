{
  pkgs,
  netboot
}:

# 1. Enable nixos/modules/dnsmasq.nix
# $ sudo iptables -F
# $ sudo nix run .#netboot-pixie-core
pkgs.writeShellScriptBin "netboot-pixie-core" ''
  set -euo pipefail
  init=$(${pkgs.gnugrep}/bin/grep -ohP 'init=\S+' ${netboot}/netboot.ipxe)
  ${pkgs.pixiecore}/bin/pixiecore \
    boot ${netboot}/bzImage ${netboot}/initrd \
    --cmdline "$init loglevel=4" \
    --debug --dhcp-no-bind --port 64172 --status-port 64172
''
