# Integration check: run the Rust test (tests/nginx_sni_routing.rs) against a
# real nginx that has the module statically linked. Unlike a NixOS VM test this
# needs no KVM — it runs in a plain build sandbox, spawning nginx on loopback.
{
  pkgs,
  lib ? pkgs.lib,
}:
let
  module = pkgs.callPackage ./package.nix { };
  nginx = pkgs.nginx.override { modules = [ module ]; };
in
pkgs.rustPlatform.buildRustPackage {
  pname = "ngx-quic-preread-nginx-test";
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

  # Build only the pure-Rust core (default features, no nginx headers needed);
  # the integration test drives nginx as an external process.
  doCheck = true;
  checkPhase = ''
    runHook preCheck
    export NGINX_BIN=${lib.getExe' nginx "nginx"}
    cargo test --test nginx_sni_routing -- --nocapture
    runHook postCheck
  '';

  installPhase = ''
    mkdir -p $out
    echo "ngx_stream_quic_preread SNI routing verified against ${nginx.name}" > $out/result
  '';

  meta = {
    description = "Runs the QUIC SNI-routing integration test against real nginx";
    platforms = lib.platforms.linux;
  };
}
