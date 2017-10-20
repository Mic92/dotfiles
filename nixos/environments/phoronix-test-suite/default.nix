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
    glibc.static
  ];
  hardeningDisable = [ "all" ];
}
