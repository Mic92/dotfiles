#!/usr/bin/env bash

set -eux -o pipefail

for v in 27 34 35 36 37 38 39 40; do 
  (
    if [[ ! -d release_$v ]]; then
      git worktree add release_$v origin/release_$v
    fi
    build=release_$v/build
    #rm -rf $build 
    mkdir -p $build
    cd $build
    ln -sf ../../build-env.nix default.nix;
    nix-shell \
      --command '$configureCommand && $buildCommand' \
      --argstr llvm_version $v
  )
done

