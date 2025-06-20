{ lib
, stdenv
, fetchFromGitHub
, darwin
}:

stdenv.mkDerivation rec {
  pname = "blueutil";
  version = "2.10.0";

  src = fetchFromGitHub {
    owner = "toy";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-XUZIE8e9shlP8RudJSyEIW4fhhV0F/cOB1mLBrqj0ek=";
  };

  buildInputs = lib.optionals stdenv.isDarwin [
    darwin.apple_sdk.frameworks.Foundation
    darwin.apple_sdk.frameworks.IOBluetooth
  ];

  makeFlags = [ "PREFIX=$(out)" ];

  meta = with lib; {
    description = "Command-line utility to control Bluetooth on macOS";
    homepage = "https://github.com/toy/blueutil";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = platforms.darwin;
    mainProgram = "blueutil";
  };
}