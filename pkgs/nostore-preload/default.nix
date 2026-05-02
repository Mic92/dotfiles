{
  lib,
  stdenv,
  patchelf,
}:
stdenv.mkDerivation (_finalAttrs: {
  pname = "nostore-preload";
  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./nostore.c
      ./test.c
    ];
  };

  nativeBuildInputs = lib.optionals (!stdenv.hostPlatform.isDarwin) [ patchelf ];

  libName = "libnostore${stdenv.hostPlatform.extensions.sharedLibrary}";

  buildPhase = ''
    runHook preBuild
    $CC -Wall -Wextra -std=c11 -O2 -fPIC -fno-stack-protector \
      nostore.c -shared -o "$libName" \
      ${lib.optionalString stdenv.hostPlatform.isDarwin "-Wl,-install_name,$out/lib/$libName"} \
      ${lib.optionalString (!stdenv.hostPlatform.isDarwin) "-ldl"}
    ${lib.optionalString (!stdenv.hostPlatform.isDarwin) ''
      # Bind to whichever libc the host process already loaded; prevents the
      # double-libc failure mode when preloaded into binaries from other
      # nixpkgs revisions.
      patchelf --remove-rpath "$libName"
    ''}
    $CC -Wall -std=c11 -O2 test.c -o test
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 "$libName" "$out/lib/$libName"
    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    mkdir -p /nix/store 2>/dev/null || true
    ${
      if stdenv.hostPlatform.isDarwin then
        ''DYLD_INSERT_LIBRARIES="$out/lib/$libName" ./test''
      else
        ''LD_PRELOAD="$out/lib/$libName" ./test''
    }
    runHook postInstallCheck
  '';

  passthru.envVar = if stdenv.hostPlatform.isDarwin then "DYLD_INSERT_LIBRARIES" else "LD_PRELOAD";

  meta = {
    description = "LD_PRELOAD guard rail blocking readdir() on /nix/store";
    platforms = lib.platforms.unix;
  };
})
