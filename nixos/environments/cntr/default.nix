with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
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
    gperftools
    # kcachegrind
    gnumake
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
