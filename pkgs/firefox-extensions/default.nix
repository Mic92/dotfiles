{
  lib,
  stdenv,
  fetchurl,
  unzip,
  zip,
  jq,
  typescript,
}:

let
  buildFirefoxExtension =
    {
      pname,
      version,
      src,
      extensionId,
      typecheck ? false,
      meta ? { },
    }:
    stdenv.mkDerivation {
      inherit pname version src;

      nativeBuildInputs = [
        unzip
        zip
        jq
      ]
      ++ lib.optional typecheck typescript;

      doCheck = typecheck;
      checkPhase = ''
        runHook preCheck
        tsc --noEmit -p jsconfig.json
        runHook postCheck
      '';

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

        # Create XPI (Firefox 62+ requires packed extensions for sideloading)
        rm -f jsconfig.json webext.d.ts
        zip -r "$out/${pname}.xpi" .

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

  app-windows-extension = buildFirefoxExtension {
    pname = "app-windows";
    version = (lib.importJSON ./app-windows/manifest.json).version;
    src = ./app-windows;
    extensionId = "app-windows@thalheim.io";
    typecheck = true;
    meta = {
      description = "Run web apps in dedicated windows; external links open in main window";
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
