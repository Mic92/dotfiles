{
  lib,
  rustPlatform,
}:

rustPlatform.buildRustPackage {
  pname = "direnv-instant";
  version = "0.1.0";

  src = ./.;

  cargoHash = "sha256-58jKfejh5QnB2LSO7EfaBryVv5+oBcW9miA8FbjKl+0=";

  meta = with lib; {
    description = "Non-blocking direnv integration daemon with tmux support";
    license = licenses.mit;
    mainProgram = "direnv-instant";
  };
}
