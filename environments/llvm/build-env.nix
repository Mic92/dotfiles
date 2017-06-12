{ llvm_version }:

with import <nixpkgs> {};

let
  cc = if llvm_version <= 27 then
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
    ccache
    utillinux # typescript
    python
  ];
  configurePhase = "cmake -B. -H.. -GNinja -DLLVM_CCACHE_BUILD=ON -DLLVM_TARGETS_TO_BUILD=X86";
  DEBUG_SYMBOLS="1";
  # only build libraries and llvm-config, this save
  # script is needed because ninja tries to reopen a tty, if stdout is not connected to one
  buildPhase = ''script -c 'ninja -t targets all' | awk -F":" '/\.a|\.so|bin\/llvm-config/ {printf "%s ", $1}' | xargs echo'';
}
