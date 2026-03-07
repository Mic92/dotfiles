{
  lib,
  rustPlatform,
  fetchFromGitHub,
  protobuf,
}:

rustPlatform.buildRustPackage {
  pname = "sediment";
  version = "0.5.0-unstable-2025-06-29";

  src = fetchFromGitHub {
    owner = "rendro";
    repo = "sediment";
    rev = "0c4629a1b8bd63707cb56694ac69f3be18e390e3";
    hash = "sha256-AsFmoBDnMPq8WWS4O04qIYl6UlN/JblDZ6Dz/vZlYF0=";
  };

  cargoLock.lockFile = ./Cargo.lock;

  postPatch = ''
    cp ${./Cargo.lock} Cargo.lock
  '';

  nativeBuildInputs = [ protobuf ];

  env = {
    PROTOC = "${protobuf}/bin/protoc";
    PROTOC_INCLUDE = "${protobuf}/include";
  };

  # Tests require network access for model downloads
  doCheck = false;

  meta = {
    description = "Semantic memory for AI agents - local-first, MCP-native";
    homepage = "https://github.com/rendro/sediment";
    license = lib.licenses.mit;
    mainProgram = "sediment";
  };
}
