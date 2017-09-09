with import <nixpkgs> {};

let
  # cc wrapper leaks unwrapped clang-3.4 into environment
  myclang = (clang_4.override {
    stdenv = (overrideCC stdenv (gcc-unwrapped.override { stripped = false; }));
  }).overrideDerivation (old: {
    buildCommand = old.buildCommand + ''
      ln -s $out/bin/clang $out/bin/clang-4.0
    '';
  });
  gefDependencies = pkgs: with pkgs; [ capstone ropper ];
in (overrideCC stdenv myclang).mkDerivation rec {
  name = "klee-unstable-2017-03-01";

  buildInputs = debuggingApps ++ [
                  jetbrains.clion
                  clang-tools
                  llvm_4
                  lldb_4
                  parallel
                  (python3.withPackages (packages:
                    with packages;
                    [ tabulate ] ++ (gefDependencies packages)
                  ))
                  perl ncurses zlib stp lit gtest
                  cryptominisat z3 ninja ];
  nativeBuildInputs = [ cmake flex bison ];
  cmakeFlags = [
    "-DENABLE_SOLVER_STP=ON"
    #"-DENABLE_SOLVER_Z3=ON"
    "-DENABLE_POSIX_RUNTIME=ON"
    "-DENABLE_KLEE_UCLIBC=ON"
    "-DKLEE_UCLIBC_PATH=../../klee-uclibc"
    "-DGTEST_SRC_DIR=${gtest.src}/googletest"
    "-DENABLE_TCMALLOC=ON"
    "-DTCMALLOC_HEADER=${gperftools}/include/gperftools/malloc_extension.h"
    "-GNinja"
    "-DCMAKE_EXPORT_COMPILE_COMMANDS=1"
  ];
  # eval cmake -B. -H.. $cmakeFlags
}
