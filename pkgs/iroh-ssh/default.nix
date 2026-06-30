{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  openssl,
}:

rustPlatform.buildRustPackage rec {
  pname = "iroh-ssh";
  version = "0.2.12";

  src = fetchFromGitHub {
    owner = "rustonbsd";
    repo = "iroh-ssh";
    rev = version;
    hash = "sha256-zt5Adq3U+sp0w5+X1xf/vEgQsxQ9G5tZL4+SOCHo5ws=";
  };

  cargoHash = "sha256-NFiOsBYJ1nJCH8E7F7WuL2Xj0/djdnvrJ/0S+9SItpo=";

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
