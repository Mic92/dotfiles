{
  lib,
  tinygo,
  stdenvNoCC,
}:
stdenvNoCC.mkDerivation {
  pname = "fluent-bit-journal-filter";
  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./filter.go
      ./go.mod
    ];
  };

  nativeBuildInputs = [ tinygo ];

  buildPhase = ''
    runHook preBuild
    export HOME=$TMPDIR
    tinygo build -o filter.wasm -target=wasi -no-debug filter.go
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm444 filter.wasm $out/lib/fluent_bit_journal_filter.wasm
    runHook postInstall
  '';
}
