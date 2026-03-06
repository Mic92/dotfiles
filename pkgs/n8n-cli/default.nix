{
  bun2nix,
  fetchFromGitHub,
  lib,
}:
let
  source = lib.importJSON ./n8n-cli-source.json;
  src = fetchFromGitHub {
    owner = "Mic92";
    repo = "n8n-cli";
    rev = source.rev;
    inherit (source) hash;
  };
in
bun2nix.mkDerivation {
  pname = "n8n-cli";
  inherit (source) version;
  inherit src;

  bunDeps = bun2nix.fetchBunDeps {
    inherit src;
    bunNix = ./bun.nix;
  };

  module = "src/index.ts";

  # Source uses top-level await which is incompatible with bytecode (CommonJS)
  bunCompileToBytecode = false;

  extraBunBuildFlags = [
    "--define"
    "CLI_VERSION='${source.version}'"
    "--define"
    "CLI_GIT_COMMIT='nix'"
  ];

  meta = {
    description = "A standalone CLI tool for managing n8n workflows";
    homepage = "https://github.com/Mic92/n8n-cli";
    license = lib.licenses.mit;
    mainProgram = "n8n-cli";
  };
}
