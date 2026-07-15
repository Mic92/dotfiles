#!/usr/bin/env bash
# Publish retiolum artefacts to retiolum.thalheim.io on push to kartei.

set -eux -o pipefail

WEBROOT=/var/www/retiolum.thalheim.io
FLAKE="git+file://$(pwd)?ref=$(git rev-parse HEAD)"

out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT

nix build --builders '' --out-link "$out/r" \
  "$FLAKE#retiolum-hosts" \
  "$FLAKE#etc-hosts" \
  "$FLAKE#etc-hosts-v6only" \
  "$FLAKE#wiregrill-json" \
  "$FLAKE#r-zone" \
  "$FLAKE#w-zone"

install -D -m644 "$out"/r-1 "$WEBROOT"/etc.hosts
install -D -m644 "$out"/r-2 "$WEBROOT"/etc.hosts-v6only
install -D -m644 "$out"/r-3 "$WEBROOT"/wiregrill.json
install -D -m644 "$out"/r-4 "$WEBROOT"/r.zone
install -D -m644 "$out"/r-5 "$WEBROOT"/w.zone
tar -cjf "$WEBROOT"/tinc-hosts.tar.bz2 -C "$out"/r --transform 's,^\.,hosts,' .
