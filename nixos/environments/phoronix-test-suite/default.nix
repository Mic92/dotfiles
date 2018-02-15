with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    php
    autoreconfHook
    popt
    libaio
    perl
    gcc7
    pcre
    glibc.out
    glibc.static
    bc
  ];
  hardeningDisable = [ "all" ];
}
