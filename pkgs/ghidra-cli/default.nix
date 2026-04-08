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
  version = "0.1.10";

  src = fetchFromGitHub {
    owner = "akiselev";
    repo = "ghidra-cli";
    rev = "v${finalAttrs.version}";
    hash = "sha256-Ci2S+eQTSyT13UEabIsaREMN7ZDMwdSCLLHAuN9KdBI=";
  };

  cargoHash = "sha256-udBEUmx9x1F1U+VDnBwMQVvwslym6gcznNJ6DSyyK2w=";

  # Upstream is broken against Ghidra 12 (see akiselev/ghidra-cli#10):
  #  - the embedded Java bridge uses the removed CParseResults API
  #  - the bridge script is dropped under ~/.config which Ghidra's Bnd-based
  #    OSGi compiler refuses to scan, so the script never gets compiled.
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
