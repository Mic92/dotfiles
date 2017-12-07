#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nodePackages.node2nix

rm -f node-env.nix
node2nix -6 -i node-packages.json -o node-packages.nix -c composition.nix
