{ llvm_version }:

with import <nixpkgs> {};

let
  cc = if llvm_version == "27" then
         gcc45
       else
         clang_4;
in (overrideCC stdenv cc).mkDerivation {
  name = "env";
  hardeningDisable = [ "all" ];
  buildInputs = [
    cmake
    bashInteractive
    ninja
  ];
  configureCommand = "cmake -B. -H.. -GNinja";
  buildCommand = "ninja";
}
