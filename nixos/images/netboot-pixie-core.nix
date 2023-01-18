{ pkgs
, netboot
}:
# 1. Enable nixos/modules/dnsmasq.nix
# $ sudo ip l set $if master internal
# $ sudo iptables -F
# $ sudo iptables -t nat -A POSTROUTING -o wlan0 -j MASQUERADE
# $ sudo nix run '.#netboot-pixie-core'
pkgs.writeShellScriptBin "netboot-pixie-core" ''
  set -euo pipefail -x
  cmdline=$(${pkgs.gnugrep}/bin/grep -ohP 'init=.+' ${netboot}/netboot.ipxe)
  cmdline=$(echo "''${cmdline//initrd=initrd /}" )
  ${pkgs.pixiecore}/bin/pixiecore \
    boot ${netboot}/bzImage ${netboot}/initrd \
    --cmdline "$cmdline" \
    --debug --dhcp-no-bind --port 64172 --status-port 64172
''
