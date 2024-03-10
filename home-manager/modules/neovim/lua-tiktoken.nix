{ fetchFromGitHub
, buildLuarocksPackage
, lua
, luarocks-build-rust-mlua
, cargo
, rustc
, rustPlatform
}:
let
  src = fetchFromGitHub {
    owner = "gptlang";
    repo = "lua-tiktoken";
    rev = "0.2.1";
    sha256 = "sha256-drSAVGHrdDdaWUEAfCE/2ZCI2nuffpbupO+TVWv/l4Y=";
  };
in
buildLuarocksPackage rec {
  pname = "tiktoken_core";
  version = "0.2.1-1";
  inherit src;
  cargoDeps = rustPlatform.fetchCargoTarball {
    inherit src;
    name = "${pname}-${version}";
    hash = "sha256-1UXTom5LpDqSOgBrGc8INq1Y0VNYZnXtavD1hIctFfs=";
  };

  buildInputs = [ cargo rustc rustPlatform.cargoSetupHook ];
  propagatedBuildInputs = [
    lua
    luarocks-build-rust-mlua
  ];
}
