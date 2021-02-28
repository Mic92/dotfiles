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
in
{
  users.users.joerg.extraGroups = [ "networkmanager" ];
  # breaks nixos-rebuild over network
  systemd.services.NetworkManager.restartIfChanged = false;
  networking.networkmanager = {
    enable = true;
    extraConfig = ''
      # using seems to make resolved ignoring all dhcp dns server
      # and therefore use out dns-over-tls resolver.
      [global-dns-domain-*]
      servers=127.0.0.53
    '';
    wifi.backend = "iwd";
    dispatcherScripts = [{
      source = "${networkmanager-hook}/bin/dispatcher";
    }];
  };
}
