{
  lib,
  stdenv,
  zip,
}:

stdenv.mkDerivation {
  pname = "browser-cli-extension";
  version = "1.0.0";

  src = ../browser-cli/extension;

  nativeBuildInputs = [ zip ];

  # Filter out development files from the extension
  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}

    # Create XPI (which is just a ZIP file)
    zip -r $out/browser-cli.xpi \
      manifest.json \
      background.js \
      content.js \
      icon.png \
      icon.svg

    # Also install to Mozilla extensions directory with the extension ID
    cp $out/browser-cli.xpi \
      "$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/browser-cli-controller@thalheim.io.xpi"

    runHook postInstall
  '';

  meta = {
    description = "Browser CLI Controller Firefox extension";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
