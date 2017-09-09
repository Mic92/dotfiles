with import <nixpkgs> {};

let
   vexRev = 3206;
   vex = stdenv.mkDerivation {
     name = "build-vex";
     src = fetchsvn {
       url = "svn://svn.valgrind.org/vex/trunk";
       rev = vexRev;
       sha256 = "1461pl5y2g2x9kv8kvdzbzi4v4rrkg9yhzndz2mmy37i5ywapm5n";
     };
     patches = [
       (fetchpatch {
          url = "https://raw.githubusercontent.com/bitblaze-fuzzball/fuzzball/6e4609bf01eb4caf0e87d247efadbd8daf102dde/vex-r3206.patch";
          sha256 = "093v2hvwsnw1ww7h558ciy16v0n5kh1142c6y08gam2p8l9z2mkf";
       })
     ];
     patchFlags = [ "-p0" ];
     makeFlags = ["-f" "Makefile-gcc"];
     installPhase = ''
       runHook preInstall
       mkdir $out
       cp -r pub libvex.a $out
       runHook postInstall
     '';
   };
   myStp = stp.overrideDerivation (old: {
     patches = [
       (fetchpatch {
          url = "https://raw.githubusercontent.com/Mic92/fuzzball/f8695b16d1d5090393605878cdb1a3de9f63e396/stp/stp-2.2.0-true-ce.patch";
          sha256 = "11vz0j10jrkvvlx0arb5xxlhay1sp51hnp0gjxmjxif0psv3dysa";
       })
     ];
     patchFlags = [ "-p0" ];
   });
in stdenv.mkDerivation {
  name = "env";
  buildInputs = with ocamlPackages; [
    bashInteractive
    autoreconfHook
    ocaml
    findlib
    camlidl
    ocaml_extlib
    valgrind
    myStp
    subversion
    vex
    binutils
    zlib
    libiberty
  ];

  #NIX_CFLAGS_COMPILE = [ "-I${camlidl}/" ];

  configureFlags = ["--with-vex=${vex}" "--with-vex-revision=${toString vexRev}"];
}
