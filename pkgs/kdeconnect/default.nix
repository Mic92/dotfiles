{
  stdenv,
  fetchurl,
  undmg,
}:

stdenv.mkDerivation rec {
  pname = "kdeconnect";
  version = "5560";

  src = fetchurl {
    url = "https://cdn.kde.org/ci-builds/network/kdeconnect-kde/master/macos-arm64/kdeconnect-kde-master-${version}-macos-clang-arm64.dmg";
    # No hash verification for nightly builds
    sha256 = "sha256-Prf47KD5XKwI3G1p6mPJ+BNUym9g+rIgCvBvvGMhcqg=";
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
