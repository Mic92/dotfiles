{
  lib,
  rustPlatform,
  fetchFromGitHub,
  ffmpeg-headless,
  pkg-config,
  openssl,
  libclang,
  clang,
}:

rustPlatform.buildRustPackage {
  pname = "route96";
  version = "0.6.0-unstable-2026-03-13";

  src = fetchFromGitHub {
    owner = "v0l";
    repo = "route96";
    rev = "7bce1a2b8d97365df62eef3a17524976665122dc";
    hash = "sha256-IQKO1v85T6I65zHDT7EpClV0lTyL+FfS3Yw7Oj2vN4o=";
  };

  cargoPatches = [
    # Remove candle (ML) and payments-rs (Lightning) optional deps that
    # break nix cargo vendoring due to broken Cargo.toml in the candle
    # monorepo.  We don't need labels or payments features.
    ./0001-remove-candle-payments-deps-for-nix-build.patch
  ];

  cargoHash = "sha256-zcoJnWNZpWQN10HLGzMuMatj4FgpaNTehwVumsBkR4M=";

  buildNoDefaultFeatures = true;
  buildFeatures = [
    "blossom"
    "nip96"
    "media-compression"
  ];

  # Only build the main binary; r96util needs optional deps (walkdir, indicatif)
  # that aren't enabled without the r96util feature.
  cargoBuildFlags = [
    "--bin"
    "route96"
  ];

  # Tests require a MySQL database
  doCheck = false;

  nativeBuildInputs = [
    pkg-config
    clang
  ];

  buildInputs = [
    ffmpeg-headless
    openssl
  ];

  env.LIBCLANG_PATH = "${libclang.lib}/lib";

  postInstall = ''
    install -Dm644 index.html $out/share/route96/index.html
  '';

  meta = {
    description = "Decentralized blob storage server with Nostr integration";
    homepage = "https://github.com/v0l/route96";
    license = lib.licenses.mit;
    mainProgram = "route96";
  };
}
