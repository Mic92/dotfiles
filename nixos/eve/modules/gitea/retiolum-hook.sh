#!/usr/bin/env bash

set -eux -o pipefail

GIT_DIR=$(pwd)
export GIT_DIR
export GIT_WORK_TREE=/var/lib/gitea/builds/stockholm
export RETIOLUM_PATH=/var/lib/gitea/builds/retiolum
export RETIOLUM_URL=gitea@git.thalheim.io:Mic92/retiolum
export WEBROOT=/var/www/retiolum.thalheim.io

export GIT_AUTHOR_NAME="gitea"
export GIT_AUTHOR_EMAIL="gitea@thalheim.io"
export GIT_COMMITTER_NAME="gitea"
export GIT_COMMITTER_EMAIL="gitea@thalheim.io"

mkdir -p $GIT_WORK_TREE
cd $GIT_WORK_TREE

git checkout -f
git submodule update --init --recursive

# Enter Directory
cat >dummy.nix <<'EOF'
{ config, lib, pkgs, ... }: {
  imports = [ ./krebs ];
  krebs = {
    enable = true;
    tinc.retiolum.enable = true;
    build.host = config.krebs.hosts.prism;
    build.user = config.krebs.users.krebs;
  };
}
EOF

cat >wiregrill.nix <<'EOF'
with import <nixpkgs/nixos> {};
with import <stockholm/lib>;
let
  self = config.krebs.build.host.nets.wiregrill;
  isRouter = !isNull self.via;
  wiregrillHosts = filterAttrs (_: h: (builtins.isAttrs h) && (hasAttr "wiregrill" h.nets)) config.krebs.hosts;
in
pkgs.writeText "hosts" (builtins.toJSON
  (mapAttrs (_: host: let
      wiregrill = host.nets.wiregrill;
    in {
    allowedIPs = if isRouter then
                   (optional (!isNull wiregrill.ip4) wiregrill.ip4.addr) ++
                   (optional (!isNull wiregrill.ip6) wiregrill.ip6.addr)
                 else
                   wiregrill.wireguard.subnets;
    publicKey = replaceStrings ["\n"] [""] wiregrill.wireguard.pubkey;
  } // optionalAttrs (!isNull wiregrill.via) {
    endpoint =  "${wiregrill.via.ip4.addr}:${toString wiregrill.wireguard.port}";
    persistentKeepalive = 61;
  }) wiregrillHosts))
EOF

nix build \
  -I secrets=./krebs/0tests/data/secrets \
  -I nixos-config=./dummy.nix \
  -I stockholm=./. \
  -f '<nixpkgs/nixos>' \
  --builders '' \
  config.krebs.tinc.retiolum.hostsArchive
install -D -m644 result $WEBROOT/tinc-hosts.tar.bz2
nix build \
  -I secrets=./krebs/0tests/data/secrets \
  -I nixos-config=./dummy.nix \
  -I stockholm=./. \
  -f '<nixpkgs/nixos>' \
  --builders '' \
  pkgs.krebs-hosts
install -D -m644 result $WEBROOT/etc.hosts
grep -E '^42:|.i |.i$' result >$WEBROOT/etc.hosts-v6only

nix-build ./wiregrill.nix \
  -I secrets=./krebs/0tests/data/secrets \
  -I nixos-config=./dummy.nix \
  -I stockholm=./. \
  --option builders ''

jq <result >$WEBROOT/wiregrill.json

unset GIT_WORK_TREE GIT_DIR

env

if [[ -d $RETIOLUM_PATH ]]; then
  cd $RETIOLUM_PATH
  git remote set-url origin $RETIOLUM_URL
  git pull origin master
else
  git clone $RETIOLUM_URL $RETIOLUM_PATH
  cd $RETIOLUM_PATH
fi

rm -rf hosts
tar -xf $WEBROOT/tinc-hosts.tar.bz2
cp -f $WEBROOT/{etc.hosts,etc.hosts-v6only,wiregrill.json} .

git add .
if [[ -z "$(git diff --staged --exit-code)" ]]; then
  echo "No changes to the output on this push; exiting."
  exit 0
fi

git commit -m "automatic update"
git push origin master
