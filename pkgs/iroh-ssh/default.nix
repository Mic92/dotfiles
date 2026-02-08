{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  openssl,
}:

rustPlatform.buildRustPackage rec {
  pname = "iroh-ssh";
  version = "0.2.8";

  src = fetchFromGitHub {
    owner = "rustonbsd";
    repo = "iroh-ssh";
    rev = version;
    hash = "sha256-jKJ0dathwsFif2N/X4CnMAG74h0h/5hnuWWwbJrbU18=";
  };

  cargoHash = "sha256-KZu4HA5E9R4sdBW5cdhyA5E2bo2YN2TPSKDlJuzDGnU=";

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
