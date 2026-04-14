{
  lib,
  swiftPackages,
  swift,
}:

swiftPackages.stdenv.mkDerivation {
  pname = "nostr-chat-bar";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ swift ];

  buildPhase = ''
    runHook preBuild
    swiftc -O -o nostr-chat-bar main.swift \
      -framework Cocoa -framework Foundation -framework Network
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    install -m 755 nostr-chat-bar $out/bin/nostr-chat-bar
    runHook postInstall
  '';

  meta = {
    description = "macOS menubar chat panel for nostr-chatd";
    license = lib.licenses.mit;
    platforms = lib.platforms.darwin;
    mainProgram = "nostr-chat-bar";
  };
}
