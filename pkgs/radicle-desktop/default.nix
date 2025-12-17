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
  pname = "radicle-desktop";
  inherit (srcs) version;

  src = fetchurl {
    inherit (srcs) url hash;
  };

  nativeBuildInputs = [ undmg ];

  sourceRoot = ".";

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/Applications"
    cp -r Radicle.app "$out/Applications/"
    runHook postInstall
  '';

  meta = {
    description = "Desktop app for Radicle, a peer-to-peer code collaboration stack";
    homepage = "https://radicle.xyz/";
    license = lib.licenses.gpl3Plus;
    platforms = [ "aarch64-darwin" ];
    mainProgram = "radicle";
  };
}
