with import <nixpkgs> {}; let
  myLlvmPackages = llvmPackages_4;
in
  stdenv.mkDerivation {
    name = "env";
    buildInputs =
      linuxPackages.perf.nativeBuildInputs
      ++ [
        elfutils
        libunwind
        libdwarf
        zlib
        binutils
        openssl
        python2
        slang
        libelf
        perl
        gtk2
        glib
        pkgconfig
        libbfd
      ]
      ++ (with llvmPackages_39; [
        clang-unwrapped
        clang
        llvm
      ]);
    hardeningDisable = ["all"];
  }
