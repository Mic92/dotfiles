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
    #extraConfig = ''
    #  # using seems to make resolved ignoring all dhcp dns server
    #  # and therefore use out dns-over-tls resolver.
    #  [global-dns-domain-*]
    #  servers=127.0.0.53
    #'';
    dispatcherScripts = [{
      source = "${networkmanager-hook}/bin/dispatcher";
    }];
  };

  # Use mac address from thinkpad network dongle also for docking station,
  # since we will not need more than two 1 Gigabyte uplinks
  systemd.network.links."00-docking-station".extraConfig = ''
    [Match]
    MACAddress = 08:3a:88:59:80:71

    [Link]
    MACAddress = 8c:8c:aa:da:9d:35
    Name = dock0
  '';
}
