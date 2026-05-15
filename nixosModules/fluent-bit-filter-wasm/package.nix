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

  postPatch = ''
    mkdir -p .cargo
    cat > .cargo/config.toml <<EOF
    [target.wasm32-wasip1]
    linker = "wasm-ld"
    EOF
  '';

  # cdylib for wasm32-wasi produces a .wasm file.
  postInstall = ''
    mkdir -p $out/lib
    find target -name '*.wasm' -exec cp {} $out/lib/ \;
  '';
}
