{
  lib,
  buildPythonApplication,
  hatchling,
  pygobject3,
  pycairo,
  grim,
  wl-clipboard,
  libnotify,
  tesseract,
  gtk4-layer-shell,
  gobject-introspection,
  wrapGAppsHook4,
  makeWrapper,
}:

buildPythonApplication {
  pname = "live-text";
  version = "0.1.0";

  src = ./.;

  pyproject = true;

  build-system = [ hatchling ];

  dependencies = [
    pygobject3
    pycairo
  ];

  nativeBuildInputs = [
    gobject-introspection
    wrapGAppsHook4
    makeWrapper
  ];

  buildInputs = [
    gtk4-layer-shell
  ];

  # Don't wrap twice (wrapGAppsHook4 + makeWrapper)
  dontWrapGApps = true;

  postFixup = ''
    wrapProgram $out/bin/live-text \
      "''${gappsWrapperArgs[@]}" \
      --prefix GI_TYPELIB_PATH : "${gtk4-layer-shell}/lib/girepository-1.0" \
      --prefix PATH : "${
        lib.makeBinPath [
          grim
          wl-clipboard
          libnotify
          tesseract
        ]
      }" \
      --set TESSDATA_PREFIX "${tesseract}/share/tessdata"
  '';

  # Tests require a Wayland session and tesseract; run manually with pytest
  doCheck = false;

  meta = {
    description = "Interactive OCR overlay for Wayland - select and copy text from your screen";
    mainProgram = "live-text";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
