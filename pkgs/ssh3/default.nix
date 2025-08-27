{
  buildGoModule,
  fetchFromGitHub,
  lib,
  libxcrypt,
}:

buildGoModule rec {
  pname = "ssh3";
  version = "0.1.6";

  src = fetchFromGitHub {
    owner = "francoismichel";
    repo = "ssh3";
    rev = "v${version}";
    hash = "sha256-A52PX24vr2TP0vWjCK34tkXqVCaPS1ebMDoQ97aXa7o=";
  };

  vendorHash = "sha256-VUNvb7m1nnH+mXUsnIKyPKJEVSMXBAaS4ihi5DZeFiI=";

  buildInputs = [ libxcrypt ];

  ldflags = [
    "-s"
    "-w"
  ];

  subPackages = [
    "cmd/ssh3"
    "cmd/ssh3-server"
  ];

  meta = with lib; {
    description = "SSH3: faster and rich secure shell using HTTP/3";
    homepage = "https://github.com/francoismichel/ssh3";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
    platforms = platforms.unix;
  };
}
