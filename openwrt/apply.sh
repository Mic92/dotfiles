#!/usr/bin/env bash

set -eux -o pipefail

run() {
    ssh "root@192.168.1.1" "$@"
}

# set unix root password
password=$(sops -d --extract '["root_password"]' secrets.yml)
{ echo $password; echo $password; } | run "passwd root"

# set-up root ssh keys
run "mkdir -p /etc/dropbear/ && umask 177 && cat > /etc/dropbear/authorized_keys" <<EOF
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine
EOF
nix run .#example --builders '' | run 'uci batch; uci commit'

# set up internet after a firmware reset
if ! run "ip link | grep -q pppoe-wan"; then
    ssh "$HOST" "/etc/init.d/network restart"
    # wait for pppoe to recover
    while ! ping -c1 -W 1 8.8.8.8; do :; done
fi

# install web interface and other packages
run "opkg update && opkg install luci tcpdump"
