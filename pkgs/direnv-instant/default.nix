{
  lib,
  rustPlatform,
}:

rustPlatform.buildRustPackage {
  pname = "direnv-instant";
  version = "0.1.0";

  src = ./.;

  cargoLock.lockFile = ./Cargo.lock;

  meta = with lib; {
    description = "Non-blocking direnv integration helper";
    license = licenses.mit;
    maintainers = [ ];
  };
}
