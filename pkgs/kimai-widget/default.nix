{
  stdenv,
  lib,
}:

stdenv.mkDerivation {
  pname = "kimai-widget";
  version = "1.0.0";

  src = ./plasmoid/com.thalheim.kimai-widget;

  installPhase = ''
    runHook preInstall
    dst="$out/share/plasma/plasmoids/com.thalheim.kimai-widget"
    mkdir -p "$dst"
    cp -r . "$dst/"
    runHook postInstall
  '';

  meta = {
    description = "KDE Plasma 6 widget showing active Kimai time tracking status";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
