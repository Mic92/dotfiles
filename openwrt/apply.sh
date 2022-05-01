#!/usr/bin/env bash

nix run .#example --builders '' | ssh -v root@192.168.1.1 'uci batch; uci commit'
