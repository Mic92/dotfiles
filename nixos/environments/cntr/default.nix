with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    rustc
    cargo
    rustracer
    rustfmt
    lldb
    fuse
    linuxPackages_4_12.perf
    bc
    gdbm
    xfsprogs
    libtool
    autoreconfHook
    pkgconfig
    m4
    bashInteractive
    dbench
  ] ++ xfstests.buildInputs
    ++ xfstests.nativeBuildInputs;
  hardeningDisable = [ "format" ];
  DUMP_OUTPUT="true";
}
