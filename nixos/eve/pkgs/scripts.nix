{ ruby
, bash
,
}:
stdenv.mkDerivation {
  name = "eve-scripts";
  src = ./scripts;
  buildInputs = [ ruby bash python3 ];
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    install -D --target $out/bin *
  '';
}
