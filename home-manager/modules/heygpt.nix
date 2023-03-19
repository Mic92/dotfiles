{ lib, pkgs, ... }:
let
  heygpt = pkgs.rustPlatform.buildRustPackage {
    name = "heygpt";
    src = pkgs.fetchFromGitHub {
      # https://github.com/fuyufjh/heygpt/pull/4
      owner = "Mic92";
      repo = "heygpt";
      rev = "84a0330364cd3669fa100f0e6241fa554fabfa6e";
      hash = "sha256-XKeGvG+KUxM2l/OplrGqY+SCEFKOWxBtIIaKjtQeyPc=";
    };
    cargoHash = "sha256-ag3QfjCZgw9qR8gjZ3KyhY+X5IEmbZD3U/ai20zVtzE=";

    buildInputs = [ pkgs.openssl ];
    nativeBuildInputs = [ pkgs.pkg-config ];

    meta = with lib; {
      description = "A simple command line tool to interact with GPT";
      homepage = "https://github.com/fuyufjh/heygpt";
      license = licenses.mit;
    };
  };
in
{
  home.packages = [ heygpt ];
}
