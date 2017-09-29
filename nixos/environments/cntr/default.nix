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
    (gdbm.overrideAttrs (old: {
      postInstall = (old.postInstall or "") + ''
        # create symlinks for compatibility
        install -dm755 $out/include/gdbm
        (
          cd $out/include/gdbm
          ln -s ../gdbm.h gdbm.h
          ln -s ../ndbm.h ndbm.h
          ln -s ../dbm.h  dbm.h
        )
      '';
    }))
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
