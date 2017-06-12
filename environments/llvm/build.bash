#!/usr/bin/env bash

set -eux -o pipefail

# supposed to executed from a checkout llvm git repository
# will attempt to build llvm versions passed to this script

if (( $# < 1 )); then
  versions=( 27 34 35 36 37 38 39 40 )
else
  versions=( $@ )
fi

for v in "${versions[@]}"; do 
  (
    if [[ ! -d release_$v ]]; then
      git worktree add release_$v origin/release_$v
    fi
    build=release_$v/build
    #rm -rf $build 
    mkdir -p $build
    cd $build
    ln -sf ../../build-env.nix default.nix
    nix-shell --option build-use-sandbox true --pure \
      --run 'eval $configurePhase && eval $buildPhase' \
      --arg llvm_version $v | tee /dev/null
  )
done

