with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    bashInteractive
    pypy27
    pypy27.pkgs.virtualenv
    libffi
    libxml2
    libxslt
    cmake
    readline
    libtool 
    glib
    pixman
    qt4
    graphviz
    binutils
    nasm
    zlib
  ];
}
