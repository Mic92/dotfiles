{
  lib,
  fetchurl,
  appimageTools,
}:

let
  pname = "sengi";
  version = "1.8.0";

  src = fetchurl {
    url = "https://github.com/NicolasConstant/sengi-electron/releases/download/v${version}/Sengi-${version}-linux.AppImage";
    sha256 = "1589wjkxgi58y7gw7vcrszchfr8y0zrwcnx5izfr9bmcvjfp13lw";
  };

  appimageContents = appimageTools.extractType2 {
    inherit pname version src;
  };
in
appimageTools.wrapType2 {
  inherit pname version src;

  extraInstallCommands = ''
    # Install desktop file and icon
    install -m 444 -D ${appimageContents}/sengi.desktop $out/share/applications/sengi.desktop
    install -m 444 -D ${appimageContents}/usr/share/icons/hicolor/256x256/apps/sengi.png \
      $out/share/pixmaps/sengi.png

    # Copy all icon sizes
    mkdir -p $out/share/icons/hicolor
    cp -r ${appimageContents}/usr/share/icons/hicolor/* $out/share/icons/hicolor/

    # Fix desktop file
    substituteInPlace $out/share/applications/sengi.desktop \
      --replace 'Exec=AppRun --no-sandbox %U' 'Exec=sengi %U'
  '';

  meta = with lib; {
    description = "Mastodon & Pleroma Multi-account Desktop Client";
    homepage = "https://nicolasconstant.github.io/sengi/";
    downloadPage = "https://github.com/NicolasConstant/sengi-electron/releases";
    license = licenses.agpl3Only;
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
    maintainers = [ ];
    platforms = [ "x86_64-linux" ];
    mainProgram = "sengi";
  };
}
