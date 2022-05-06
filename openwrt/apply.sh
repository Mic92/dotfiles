#!/usr/bin/env bash

set -eux -o pipefail

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
HOST="192.168.1.1"

run() {
    ssh "root@$HOST" "$@"
}

# Set unix root password
password=$(sops -d --extract '["root_password"]' secrets.yml)
{ echo $password; echo $password; } | run "passwd root"

# Set root ssh keys
run "mkdir -p /etc/dropbear/ && umask 177 && cat > /etc/dropbear/authorized_keys" <<EOF
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine
EOF

# Apply uci configuration
nix run "./#example" | run 'uci batch; uci commit'

# Set up internet after a firmware reset
if ! run "ip link | grep -q pppoe-wan"; then
    ssh "$HOST" "/etc/init.d/network restart"
    # wait for pppoe to recover
    while ! ping -c1 -W 1 8.8.8.8; do :; done
fi

# Install web interface and other packages
run "opkg update && opkg install luci tcpdump tinc rsync ddns-scripts-nsupdate iperf3"

run "if [ ! -f /etc/tinc/retiolum/rsa_key.priv ]; then mkdir -p /etc/tinc/retiolum; tinc -n retiolum generate-keys; /etc/init.d/tinc start; fi"
rsync -e ssh -ac /etc/tinc/retiolum/hosts "root@$HOST:/etc/tinc/retiolum"
