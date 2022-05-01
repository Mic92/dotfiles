#!/usr/bin/env bash

URL=https://downloads.openwrt.org/snapshots/targets/mediatek/mt7622/openwrt-mediatek-mt7622-linksys_e8450-ubi-squashfs-sysupgrade.itb
HOST="192.168.1.1"
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

run() {
    ssh "root@$HOST" "$@"
}

wget "$URL" -O openwrt.sysupgrade.itb
#scp openwrt.sysupgrade.itb "root@$HOST:/tmp/openwrt.sysupgrade.itb"
#run "sysupgrade -v /tmp/openwrt.sysupgrade.itb"
## wait for router to come back
#while ! nc -zv "$HOST" 22; do sleep 2; done
#"$SCRIPT_DIR/apply.sh"
