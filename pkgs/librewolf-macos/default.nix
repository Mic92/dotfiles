{
  lib,
  stdenv,
  fetchurl,
  undmg,
}:

stdenv.mkDerivation rec {
  pname = "librewolf";
  version = "146.0-2";

  src = fetchurl {
    url = "https://gitlab.com/api/v4/projects/44042130/packages/generic/librewolf/${version}/librewolf-${version}-macos-arm64-package.dmg";
    hash = "sha256-NW9DgHcaJYkVagf4lylLR8qTsJ6NiiGuLhZIsSCOMDM=";
  };

  nativeBuildInputs = [ undmg ];

  sourceRoot = ".";

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/Applications"
    cp -r LibreWolf.app "$out/Applications/"
    runHook postInstall
  '';

  meta = {
    description = "A privacy-focused fork of Firefox";
    homepage = "https://librewolf.net/";
    license = lib.licenses.mpl20;
    platforms = [ "aarch64-darwin" ];
    mainProgram = "librewolf";
  };
}
