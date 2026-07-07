# nginx module descriptor for `services.nginx.additionalModules` /
# `nginx.override { modules = [ ... ]; }`.
#
# This is *not* a normal derivation: it is the attrset the nginx build consumes.
# The module is compiled statically into nginx via `--add-module`; nginx's build
# invokes cargo through config.make / auto/rust (vendored from ngx-rust). No
# `--with-compat` and no dynamic .so are involved.
{
  lib,
  cargo,
  rustc,
  rustPlatform,
}:
let
  vendoredDeps = rustPlatform.importCargoLock { lockFile = ./Cargo.lock; };
in
{
  name = "ngx-stream-quic-preread";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.toml
      ./Cargo.lock
      ./auto
      ./config
      ./config.make
      ./src
    ];
  };

  # Added to nginx's build environment. bindgenHook provides libclang for
  # nginx-sys' rust-bindgen invocation.
  inputs = [
    cargo
    rustc
    rustPlatform.bindgenHook
  ];

  # Point cargo at the vendored crate sources and build fully offline, so the
  # nginx build needs no network.
  preConfigure = ''
    export CARGO_HOME=$TMPDIR/cargo-home
    mkdir -p "$CARGO_HOME"
    cat > "$CARGO_HOME/config.toml" <<EOF
    [source.crates-io]
    replace-with = "vendored-sources"

    [source.vendored-sources]
    directory = "${vendoredDeps}"
    EOF
    export NGX_RUSTC_OPT="--offline"
  '';

  meta = {
    description = "nginx stream module: preread SNI/ALPN from QUIC Initial packets (QUIC ssl_preread)";
    license = with lib.licenses; [
      mit
      asl20
    ];
    platforms = lib.platforms.linux;
  };
}
