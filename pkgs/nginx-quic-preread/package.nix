{
  lib,
  stdenv,
  rustPlatform,
  # nginx to build the module against. The produced .so is loadable into a
  # matching nginx built `--with-compat` (see nixosModules/nginx-quic-preread.nix).
  nginx,
}:

rustPlatform.buildRustPackage {
  pname = "ngx-stream-quic-preread";
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

  # nginx-sys generates bindings via rust-bindgen -> needs libclang.
  nativeBuildInputs = [ rustPlatform.bindgenHook ];

  # nginx-sys reads the headers and `objs/` of a *configured* nginx source tree.
  # We only need the `./configure` step (which generates ngx_auto_config.h and
  # the Makefile bindgen parses); no `make` is required. `--with-compat` pins the
  # module signature so the resulting .so loads into any same-version nginx also
  # built with `--with-compat`, and `--with-stream` exposes the ngx_stream_* API.
  preConfigure = ''
    mkdir -p nginx-src
    if [ -d ${nginx.src} ]; then
      cp -r ${nginx.src}/. nginx-src/
    else
      tar xf ${nginx.src} -C nginx-src --strip-components=1
    fi
    chmod -R u+w nginx-src
    export NGINX_SOURCE_DIR="$PWD/nginx-src"
    (
      cd nginx-src
      ./configure \
        --with-compat \
        --with-stream \
        --without-http_rewrite_module \
        --without-http_gzip_module
    )
  '';

  # The crate is a cdylib; buildRustPackage has no binary to install by default,
  # so place the module under the conventional nginx modules path ourselves.
  postInstall = ''
    mkdir -p $out/lib/nginx/modules
    find target -name 'libngx_quic_preread.so' -exec \
      cp {} $out/lib/nginx/modules/ngx_stream_quic_preread_module.so \;
  '';

  meta = {
    description = "nginx stream module: preread SNI/ALPN from QUIC Initial packets (QUIC ssl_preread)";
    longDescription = ''
      Registers a stream preread-phase handler and the variables
      $quic_preread_server_name and $quic_preread_alpn_protocols by decrypting
      the QUIC Initial packet (keys derived from the Destination Connection ID,
      RFC 9001) and parsing the TLS ClientHello. Lets nginx route QUIC/UDP by SNI
      without terminating TLS, the QUIC analogue of the built-in ssl_preread.
    '';
    license = with lib.licenses; [
      mit
      asl20
    ];
    platforms = lib.platforms.linux;
  };
}
