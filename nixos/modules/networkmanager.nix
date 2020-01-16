{ pkgs, lib, ... }:

with pkgs;
let
  networkmanager-hook = stdenv.mkDerivation {
    name = "networkmanager-hook";
    src = ./networkmanager.py;
    buildInputs = [ python3 ];
    nativeBuildInputs = [ makeWrapper python3.pkgs.mypy ];
    dontUnpack = true;
    installPhase = ''
      install -D -m755 $src $out/bin/dispatcher
      mypy $src
      wrapProgram $out/bin/dispatcher --prefix PATH : ${lib.makeBinPath [ systemd alsaUtils ]}
    '';
  };
in {
  networking.networkmanager = {
    enable = true;
    dispatcherScripts = [{
      source = "${networkmanager-hook}/bin/dispatcher";
    }];
  };
}
