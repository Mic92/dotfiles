# The Rust crate. A normal buildRustPackage: its default `cargo test` checkPhase
# runs the unit tests and the integration test (tests/nginx_sni_routing.rs),
# the latter driving the real nginx built with the module via $NGINX_BIN.
{
  lib,
  rustPlatform,
  nginx,
  callPackage,
}:
let
  nginxWithModule = nginx.override {
    modules = [ (callPackage ./package.nix { }) ];
  };
in
rustPlatform.buildRustPackage {
  pname = "ngx-quic-preread";
  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.toml
      ./Cargo.lock
      ./src
      ./tests
    ];
  };
  cargoLock.lockFile = ./Cargo.lock;

  env.NGINX_BIN = lib.getExe' nginxWithModule "nginx";

  postInstall = "mkdir -p $out";

  meta = {
    description = "QUIC Initial SNI/ALPN preread core for the nginx stream module";
    license = with lib.licenses; [
      mit
      asl20
    ];
    platforms = lib.platforms.linux;
  };
}
