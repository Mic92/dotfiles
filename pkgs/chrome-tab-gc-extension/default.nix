{
  lib,
  stdenv,
  fetchurl,
  unzip,
  zip,
  jq,
}:

stdenv.mkDerivation rec {
  pname = "chrome-tab-gc-extension";
  version = "1.2";

  src = fetchurl {
    url = "https://github.com/Mic92/chrome-tab-gc/releases/download/${version}/tab_garbage_collector-${version}.xpi";
    hash = "sha256-O5+cevbcaHcmwcD2jl5J/Dqxk67tQm8ks0+yrK0n1YE=";
  };

  nativeBuildInputs = [
    unzip
    zip
    jq
  ];

  # Extension ID to use for policy-based installation
  extensionId = "tab-garbage-collector@thalheim.io";

  unpackPhase = ''
    mkdir -p source
    unzip $src -d source
  '';

  buildPhase = ''
    cd source
    # Add extension ID to manifest for policy-based installation
    jq '. + {"browser_specific_settings": {"gecko": {"id": "${extensionId}"}}}' \
      manifest.json > manifest.json.tmp
    mv manifest.json.tmp manifest.json
  '';

  installPhase = ''
    runHook preInstall

    # Create XPI with updated manifest
    zip -r $out/chrome-tab-gc.xpi .

    # Create extracted extension directory for auto-install
    mkdir -p "$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/${extensionId}"
    cp -r . "$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/${extensionId}/"

    runHook postInstall
  '';

  meta = {
    description = "Tab Garbage Collector - closes tabs that have not been viewed for long time";
    homepage = "https://github.com/Mic92/chrome-tab-gc";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
