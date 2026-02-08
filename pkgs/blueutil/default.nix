{
  lib,
  stdenv,
  fetchFromGitHub,
}:

let
  srcs = lib.importJSON ./srcs.json;
in
stdenv.mkDerivation {
  pname = "blueutil";
  inherit (srcs) version;
  src = fetchFromGitHub {
    owner = "toy";
    repo = "blueutil";
    rev = "v${srcs.version}";
    hash = srcs.hash;
  };
  makeFlags = [
    "PREFIX=$(out)"
  ];
  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    install -m 755 blueutil $out/bin/blueutil

    runHook postInstall
  '';
  NIX_LDFLAGS = lib.optionals stdenv.isDarwin [
    "-framework Foundation"
    "-framework IOBluetooth"
  ];
  meta = with lib; {
    description = "Command-line utility to control Bluetooth on macOS";
    homepage = "https://github.com/toy/blueutil";
    license = licenses.mit;
    platforms = platforms.darwin;
    mainProgram = "blueutil";
    longDescription = ''
      blueutil is a command-line utility for macOS that allows you to control Bluetooth.

      Usage:
        blueutil -p       # Show current Bluetooth power state (0=off, 1=on)
        blueutil -p 1     # Turn Bluetooth on
        blueutil -p 0     # Turn Bluetooth off
        blueutil -p toggle # Toggle Bluetooth power
    '';
  };
}
