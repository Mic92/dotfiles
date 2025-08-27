{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  openssl,
}:

rustPlatform.buildRustPackage {
  pname = "iroh-ssh";
  version = "unstable-2025-01-27";

  src = fetchFromGitHub {
    owner = "rustonbsd";
    repo = "iroh-ssh";
    rev = "318f16864e123b2062ba34b0fd4f5145d3ae7bde";
    hash = "sha256-5om1FER8nMFrCm50Tfk/vGlHn8pR5129E5ekrPQtCmM=";
  };

  cargoHash = "sha256-BlTeCuFAWiKV4i2CQNbZQqw8p3w8ce9b8Q/fG0bQ7XI=";

  nativeBuildInputs = [ pkg-config ];

  buildInputs = [ openssl ];

  meta = with lib; {
    description = "SSH server and client built on Iroh networking";
    homepage = "https://github.com/rustonbsd/iroh-ssh";
    license = with licenses; [
      mit
      asl20
    ];
    mainProgram = "iroh-ssh";
  };
}
