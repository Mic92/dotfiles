{ stdenv, kmod }:
stdenv.mkDerivation {
  name = "toggle-keyboard";
  src = ./.;
  buildPhase = ''
    mkdir -p $out/bin
    substituteInPlace toggle-keyboard.c \
      --replace @modprobe@ ${kmod}/bin/modprobe \
      --replace @rmmod@ ${kmod}/bin/rmmod
    $CC $CFLAGS $LDFLAGS -o $out/bin/toggle-keyboard toggle-keyboard.c
  '';
}
