{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  wrapGAppsHook3,
  webkitgtk_4_1,
  gtk3,
  libayatana-appindicator,
  librsvg,
  openssl,
  libsoup_3,
  xdotool,
  makeWrapper,
  stdenv,
  darwin,
  bun,
  cacert,
  rustfmt,
  claude-code,
}:

let
  src = fetchFromGitHub {
    owner = "getAsterisk";
    repo = "claudia";
    rev = "ccd95330cb2949fc5b2aae868764bfbc4cbe2deb";
    hash = "sha256-y8uumwuvlW/pEm7I9wElTx8nr/zJO9aISAn7JKkDNVk=";
  };

  # Fixed output derivation for Bun dependencies
  bunDeps = stdenv.mkDerivation {
    pname = "claudia-bun-deps";
    version = "0.1.0";

    inherit src;

    nativeBuildInputs = [ bun ];

    dontBuild = true;
    dontFixup = true;

    outputHashMode = "recursive";
    outputHash = "sha256-vMoK454iFOqs7KMCPPbzkfsv6Cv1qkuOWfbMVEsYHqs=";

    SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";

    installPhase = ''
      runHook preInstall

      export HOME=$(mktemp -d)
      bun install --frozen-lockfile --no-progress

      # Copy both node_modules and bun.lock
      mkdir -p $out
      cp -r node_modules $out/
      cp bun.lock $out/

      runHook postInstall
    '';
  };
in
rustPlatform.buildRustPackage rec {
  pname = "claudia";
  version = "0.1.0";

  inherit src;

  sourceRoot = "${src.name}/src-tauri";

  cargoHash = "sha256-I4j9YBv+EOsMQj9nj3GPWCR2NMBM0XZ5UJpCKCIpKm4=";

  nativeBuildInputs =
    [
      pkg-config
      wrapGAppsHook3
      makeWrapper
      bun
      rustfmt
    ]
    ++ lib.optionals stdenv.isDarwin [
      rustPlatform.bindgenHook
    ];

  buildInputs =
    [
      openssl
    ]
    ++ lib.optionals stdenv.isLinux [
      webkitgtk_4_1
      gtk3
      libayatana-appindicator
      librsvg
      libsoup_3
      xdotool
    ]
    ++ lib.optionals stdenv.isDarwin [
      darwin.apple_sdk.frameworks.AppKit
      darwin.apple_sdk.frameworks.CoreGraphics
      darwin.apple_sdk.frameworks.Foundation
      darwin.apple_sdk.frameworks.Security
      darwin.apple_sdk.frameworks.WebKit
    ];

  # Override the Tauri build command since we pre-build the frontend
  TAURI_SKIP_DEVSERVER_CHECK = "true";

  preBuild = ''
    # Copy the entire source tree to a writable location
    cd ..
    chmod -R u+w .

    # Copy dependencies
    cp -r ${bunDeps}/node_modules .
    cp ${bunDeps}/bun.lock .
    chmod -R u+w node_modules

    export HOME=$(mktemp -d)
    # Run build commands directly
    ${bun}/bin/bun run node_modules/typescript/bin/tsc
    ${bun}/bin/bun run node_modules/vite/bin/vite.js build

    # Go back to src-tauri for the Rust build
    cd src-tauri
  '';

  postInstall = ''
    # Install desktop file on Linux
    ${lib.optionalString stdenv.isLinux ''
      install -Dm644 $src/src-tauri/icons/icon.png $out/share/icons/hicolor/512x512/apps/claudia.png

      mkdir -p $out/share/applications
      cat > $out/share/applications/claudia.desktop <<EOF
      [Desktop Entry]
      Name=Claudia
      Comment=A powerful GUI app and Toolkit for Claude Code
      Exec=$out/bin/claudia
      Icon=claudia
      Type=Application
      Categories=Development;Utility;
      Terminal=false
      EOF
    ''}

    # Wrap the binary to ensure Claude Code CLI is available
    wrapProgram $out/bin/claudia \
      --prefix PATH : ${lib.makeBinPath [ claude-code ]}
  '';

  doCheck = false; # Tests require display

  meta = with lib; {
    description = "A powerful GUI app and Toolkit for Claude Code";
    homepage = "https://github.com/getAsterisk/claudia";
    license = licenses.agpl3Only;
    maintainers = with maintainers; [ ]; # Add your name here if you want
    platforms = platforms.unix;
    mainProgram = "claudia";
  };
}
