{
  pkgs ?
    import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-23.11.tar.gz")
      { },
}:
#{ pkgs ? import <nixpkgs> { } }:
pkgs.stdenvNoCC.mkDerivation {
  name = "env";
  nativeBuildInputs = [ pkgs.apache-directory-studio ];
}
