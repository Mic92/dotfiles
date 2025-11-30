{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage {
  pname = "phantun";
  version = "0.8.1";

  src = fetchFromGitHub {
    owner = "dndx";
    repo = "phantun";
    rev = "v0.8.1";
    hash = "sha256-Syz2W0IYm9nzu2Ph09uJxeo/odznDC9aMe+TEMhWurc=";
  };

  cargoHash = "sha256-xvWk1OmF/oWc2Pw3rxkkRNZ63XSkSf41klzQz1+2O9I=";

  meta = with lib; {
    description = "Transforms UDP stream into (fake) TCP streams";
    homepage = "https://github.com/dndx/phantun";
    license = with licenses; [
      mit
      asl20
    ];
  };
}
