{
  fetchFromGitHub,
  buildLuarocksPackage,
  lua,
  luarocks-build-rust-mlua,
  cargo,
  rustc,
  rustPlatform,
}:
let
  src = fetchFromGitHub {
    owner = "gptlang";
    repo = "lua-tiktoken";
    rev = "v0.2.2";
    sha256 = "sha256-H83kk9dsH/cWBEx2AXQQ82l8sNfhzO864jwDd7vwAQc=";
  };
in
buildLuarocksPackage rec {
  pname = "tiktoken_core";
  version = "0.2.2-1";
  inherit src;
  cargoDeps = rustPlatform.fetchCargoTarball {
    inherit src;
    name = "${pname}-${version}";
    hash = "sha256-6dC+3x91Itp3jkqH/mXlwMfhQ/E3D52dxpv+7f/0fyU=";
  };

  buildInputs = [
    cargo
    rustc
    rustPlatform.cargoSetupHook
  ];
  propagatedBuildInputs = [
    lua
    luarocks-build-rust-mlua
  ];
}
