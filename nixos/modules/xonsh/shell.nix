with import <nixpkgs> {};
  mkShell {
    buildInputs = [poetry];
  }
