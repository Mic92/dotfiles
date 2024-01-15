{ stdenv }:
stdenv.mkDerivation {
  name = "toggle-keyboard";
  src = ./.;
  buildPhase = ''
    mkdir -p $out/bin
    $CC $CFLAGS $LDFLAGS -o $out/bin/toggle-keyboard toggle-keyboard.c
  '';
}
