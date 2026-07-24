{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage {
  pname = "herdr-pluck";
  version = "0.1.0-unstable-2026-07-23";

  src = fetchFromGitHub {
    owner = "Mic92";
    repo = "herdr-pluck";
    # osc52 branch: OSC 52 clipboard fallback
    rev = "6f94c5b2e41e3f51a868847d7a62f140c4ff496c";
    hash = "sha256-7MyNBAHUbimRd68Oj8d9Y2l4knmHMqHNNdUtBJOkwJM=";
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
