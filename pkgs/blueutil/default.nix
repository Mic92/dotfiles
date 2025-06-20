{
  lib,
  stdenv,
  fetchFromGitHub,
  darwin,
}:

stdenv.mkDerivation rec {
  pname = "blueutil";
  version = "2.10.0";

  src = fetchFromGitHub {
    owner = "toy";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-x2khx8Y0PolpMiyrBatT2aHHyacrQVU/02Z4Dz9fBtI=";
  };

  buildInputs = lib.optionals stdenv.isDarwin [
    darwin.apple_sdk.frameworks.Foundation
    darwin.apple_sdk.frameworks.IOBluetooth
  ];

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
    maintainers = with maintainers; [ ];
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
