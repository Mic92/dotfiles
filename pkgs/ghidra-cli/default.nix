{
  lib,
  rustPlatform,
  fetchFromGitHub,
  callPackage,
  makeWrapper,
  pkg-config,
  openssl,
  ghidra,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "ghidra-cli";
  version = "0.2.2";

  src = fetchFromGitHub {
    owner = "akiselev";
    repo = "ghidra-cli";
    rev = "v${finalAttrs.version}";
    hash = "sha256-B4bnOOFtEsckT5TOAmjbx5AkrdpjeA248G+BrDUHY88=";
  };

  cargoHash = "sha256-r8AvlTJQ+j5YoLGJe3xIA0q+DPDTMKfhlT+nwFfNsPw=";

  # - The bridge script is dropped under ~/.config which Ghidra's Bnd-based
  #   OSGi compiler refuses to scan (akiselev/ghidra-cli#10).
  # - Ghidra 12.2 dropped DataTypeManager.remove(DataType, TaskMonitor).
  patches = [ ./ghidra-12-compat.patch ];

  nativeBuildInputs = [
    makeWrapper
    pkg-config
  ];

  buildInputs = [ openssl ];

  # Integration tests require a running Ghidra installation / network.
  checkFlags = [
    "--skip=tests::"
  ];
  doCheck = false;

  passthru.tests.decompile = callPackage ./test.nix {
    ghidra-cli = finalAttrs.finalPackage;
  };

  postInstall = ''
    # Point the CLI at the Nix-provided Ghidra so users don't need `setup`.
    wrapProgram $out/bin/ghidra \
      --set-default GHIDRA_INSTALL_DIR ${ghidra}/lib/ghidra
  '';

  meta = with lib; {
    description = "Run Ghidra headless from the CLI for reverse engineering with coding agents";
    homepage = "https://github.com/akiselev/ghidra-cli";
    license = licenses.gpl3Only;
    mainProgram = "ghidra";
  };
})
