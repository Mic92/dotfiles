{
  lib,
  buildPythonApplication,
  hatchling,
  pygobject3,
  pycairo,
  rapidocr,
  grim,
  slurp,
  wl-clipboard,
  libnotify,
  gtk4-layer-shell,
  gobject-introspection,
  wrapGAppsHook4,
  makeWrapper,
  fetchurl,
}:

let
  # English recognition model — preserves spaces in Latin text, unlike the
  # bundled Chinese model which strips whitespace.
  en-rec-model = fetchurl {
    url = "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.5.0/onnx/PP-OCRv4/rec/en_PP-OCRv4_rec_infer.onnx";
    hash = "sha256-6HcMlnYFmD0VcM31NSBB37aPoMIWZPSfR7FVq9Pg4xg=";
  };
in
buildPythonApplication {
  pname = "live-text";
  version = "0.1.0";

  src = ./.;

  pyproject = true;

  build-system = [ hatchling ];

  dependencies = [
    pygobject3
    pycairo
    rapidocr
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
          slurp
          wl-clipboard
          libnotify
        ]
      }" \
      --set LIVE_TEXT_REC_MODEL "${en-rec-model}"
  '';

  # Tests require a Wayland session; run manually with pytest
  doCheck = false;

  meta = {
    description = "Interactive OCR overlay for Wayland - select and copy text from your screen";
    mainProgram = "live-text";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
