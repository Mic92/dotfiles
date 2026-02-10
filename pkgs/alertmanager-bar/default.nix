{
  lib,
  swiftPackages,
  swift,
}:

swiftPackages.stdenv.mkDerivation {
  pname = "alertmanager-bar";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ swift ];

  buildPhase = ''
    runHook preBuild
    swiftc -O -o alertmanager-bar main.swift -framework Cocoa -framework Foundation
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    install -m 755 alertmanager-bar $out/bin/alertmanager-bar
    runHook postInstall
  '';

  meta = {
    description = "macOS menu bar counter for Alertmanager alerts";
    license = lib.licenses.mit;
    platforms = lib.platforms.darwin;
    mainProgram = "alertmanager-bar";
  };
}
