{ lib
, rustPlatform
, fetchFromGitHub
, pkg-config
, cairo
, glib
, pango
}:

rustPlatform.buildRustPackage {
  pname = "i3bar-river";
  version = "unstable-2023-08-21";

  src = fetchFromGitHub {
    owner = "MaxVerevkin";
    repo = "i3bar-river";
    rev = "f27d993243848727249e083f3dc13c471ca856e1";
    hash = "sha256-NU8AYZ30awzFIOWsajs1w0lQMg8O9G2AGDiKEpfBi/g=";
  };

  cargoHash = "sha256-ekovq0yBkT01U3aLktgrzA6KZoAyn05GB7wok4rIwHE=";

  nativeBuildInputs = [
    pkg-config
  ];

  buildInputs = [
    cairo
    glib
    pango
  ];

  meta = with lib; {
    description = "A port of i3bar for river";
    homepage = "https://github.com/MaxVerevkin/i3bar-river";
    license = licenses.gpl3Only;
  };
}
