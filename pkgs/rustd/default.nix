{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage {
  pname = "rustd";
  version = "0-unstable-2026-10-21";

  src = fetchFromGitHub {
    owner = "DioxusLabs";
    repo = "rustd";
    rev = "ebcd234a02f769e380875b8b80e18164bba1b353";
    # vendor/rust-analyzer is a submodule of the same repository
    fetchSubmodules = true;
    hash = "sha256-TbuxlNpAiB+rp2Rdu66/khFUuLiLc4UIRbejTqP1BIg=";
  };

  cargoHash = "sha256-n0w8mn9wrFny8NerXUFnTu9/O08j1T5NItT1HkTHolo=";

  cargoBuildFlags = [
    "-p"
    "rustd"
  ];

  # Tests compare against a rustc toolchain with rust-src at runtime
  doCheck = false;

  meta = {
    description = "Rust compiler daemon for faster incremental cargo builds";
    homepage = "https://github.com/DioxusLabs/rustd";
    license = with lib.licenses; [
      mit
      asl20
    ];
    mainProgram = "cargo-rustd";
  };
}
