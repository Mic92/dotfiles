{
  lib,
  stdenv,
  fetchurl,
  unzip,
  zip,
  jq,
}:

let
  buildFirefoxExtension =
    {
      pname,
      version,
      src,
      extensionId,
      meta ? { },
    }:
    stdenv.mkDerivation {
      inherit pname version src;

      nativeBuildInputs = [
        unzip
        zip
        jq
      ];

      unpackPhase = ''
        mkdir -p source
        if [[ "$src" == *.xpi ]] || file "$src" | grep -q "Zip archive"; then
          unzip "$src" -d source
        else
          cp -r "$src"/* source/
        fi
      '';

      buildPhase = ''
        cd source
        # Ensure extension ID is set in manifest
        if ! jq -e '.browser_specific_settings.gecko.id' manifest.json >/dev/null 2>&1; then
          jq '. + {"browser_specific_settings": {"gecko": {"id": "${extensionId}"}}}' \
            manifest.json > manifest.json.tmp
          mv manifest.json.tmp manifest.json
        fi
      '';

      installPhase = ''
        runHook preInstall

        mkdir -p "$out"

        # Create XPI
        zip -r "$out/${pname}.xpi" .

        # Create extracted extension directory for sideloading
        # Firefox GUID: {ec8030f7-c20a-464f-9b0e-13a3a9e97384}
        mkdir -p "$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/${extensionId}"
        cp -r . "$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/${extensionId}/"

        runHook postInstall
      '';

      meta = {
        platforms = lib.platforms.all;
      }
      // meta;
    };
in
{
  inherit buildFirefoxExtension;

  browser-cli-extension = buildFirefoxExtension {
    pname = "browser-cli-extension";
    version = "1.0.0";
    src = ../browser-cli/extension;
    extensionId = "browser-cli-controller@thalheim.io";
    meta = {
      description = "Browser CLI Controller Firefox extension";
      license = lib.licenses.mit;
    };
  };

  chrome-tab-gc-extension = buildFirefoxExtension {
    pname = "chrome-tab-gc-extension";
    version = "1.2";
    src = fetchurl {
      url = "https://github.com/Mic92/chrome-tab-gc/releases/download/1.2/tab_garbage_collector-1.2.xpi";
      hash = "sha256-vXGjpHHT95g3Am5b3YGCZv4GKK6MV0uV5aPsAo4QT7g=";
    };
    extensionId = "tab-garbage-collector@thalheim.io";
    meta = {
      description = "Tab Garbage Collector - closes tabs that have not been viewed for long time";
      homepage = "https://github.com/Mic92/chrome-tab-gc";
      license = lib.licenses.mit;
    };
  };
}
