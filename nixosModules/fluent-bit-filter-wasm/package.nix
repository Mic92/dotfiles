{
  pkgsCross,
  lib,
  buildPackages,
}:
let
  wasiPkgs = pkgsCross.wasi32;
in
wasiPkgs.rustPlatform.buildRustPackage {
  pname = "fluent-bit-journal-filter";
  version = "0.1.0";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.toml
      ./Cargo.lock
      ./src
    ];
  };

  cargoLock.lockFile = ./Cargo.lock;

  nativeBuildInputs = [ buildPackages.lld ];

  # nix cargo hooks set CARGO_TARGET_WASM32_WASIP1_LINKER to clang wrapper,
  # but rustc passes wasm-ld flags (-flavor wasm, --export, etc.) directly
  # to the linker, which clang doesn't understand. RUSTFLAGS -Clinker
  # takes precedence over the env var.
  RUSTFLAGS = "-Clinker=${buildPackages.lld}/bin/wasm-ld";

  # cdylib for wasm32-wasi produces a .wasm file.
  postInstall = ''
    mkdir -p $out/lib
    find target -name '*.wasm' -exec cp {} $out/lib/ \;
  '';
}
