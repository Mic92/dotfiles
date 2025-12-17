{
  lib,
  stdenv,
  fetchurl,
  undmg,
}:

let
  srcs = lib.importJSON ./srcs.json;
in
stdenv.mkDerivation {
  pname = "kdeconnect";
  inherit (srcs) version;

  src = fetchurl {
    inherit (srcs) url hash;
  };

  nativeBuildInputs = [ undmg ];

  sourceRoot = ".";

  installPhase = ''
    mkdir -p "$out/Applications"
    cp -r "KDE Connect.app" "$out/Applications/"
  '';

  meta = {
    description = "KDE Connect - Multi-platform app for wireless integration between devices";
    homepage = "https://kdeconnect.kde.org/";
    platforms = [ "aarch64-darwin" ];
  };
}
