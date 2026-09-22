{
  lib,
  buildGoModule,
  fetchFromGitHub,
}:

buildGoModule rec {
  pname = "goatcounter";
  version = "2.7.0";

  src = fetchFromGitHub {
    owner = "zgoat";
    repo = "goatcounter";
    rev = "v${version}";
    hash = "sha256-BVKz1vPYDOpaBdurR1wjg1Jo+qncqrIbZuDVmlFAO9I=";
  };

  subPackages = [ "cmd/goatcounter" ];

  ldflags = [ "-X=main.Version=${version}" ];

  doCheck = false;

  vendorHash = "sha256-ICbtL6O1Kbig2WgbLCVSSe9MbIrYWmxYn4M2R5mwv/c=";

  meta = {
    description = "Easy web analytics. No tracking of personal data.";
    homepage = "https://www.goatcounter.com/";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ mic92 ];
    platforms = lib.platforms.unix;
    mainProgram = "goatcounter";
  };
}
