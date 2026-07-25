{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage {
  pname = "herdr-pluck";
  version = "0.1.0-unstable-2026-07-25";

  src = fetchFromGitHub {
    owner = "Mic92";
    repo = "herdr-pluck";
    # osc52 branch: OSC 52 clipboard fallback + clipboard backend config option
    rev = "7743d2d4aafd974df652b10559f48613024f4817";
    hash = "sha256-Qz3YD5j5+wuvc3bF1Za99yrUD323aM35x4ZbimjEzlQ=";
  };

  cargoHash = "sha256-h3yU5gPuJSdv4fW8kbfCxdAR0Nnnr5/dYTNaMhNNFIE=";

  # Ship as a herdr plugin directory: manifest at the root, binary under bin/,
  # matching the "./bin/herdr-pluck" action command in the manifest. The
  # manifest's [[build]] binary download is not used; herdr plugin link does
  # not run build commands.
  postInstall = ''
    cp herdr-plugin.toml $out/
  '';

  meta = {
    description = "Inline keyboard hints for copying visible terminal tokens from Herdr panes";
    homepage = "https://github.com/rmarganti/herdr-pluck";
    license = lib.licenses.mit;
    mainProgram = "herdr-pluck";
  };
}
