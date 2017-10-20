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
    bc
    gdbm
    xfsprogs
    libtool
    autoreconfHook
    pkgconfig
    m4
    bashInteractive
    dbench
    fio
    glibc.debug
    latest.rustChannels.nightly.rust
    latest.rustChannels.nightly.cargo

    (e2fsprogs.overrideDerivation (old: {
      patches = [./0001-fix-stackoverflow-in-fgetversion.patch];
    }))
  ] ++ xfstests.buildInputs
    ++ xfstests.nativeBuildInputs;
  hardeningDisable = [ "format" ];
  DUMP_OUTPUT="true";
}
