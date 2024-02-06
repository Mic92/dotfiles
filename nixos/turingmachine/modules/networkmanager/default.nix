{ pkgs
, lib
, ...
}:
with pkgs; let
  networkmanager-hook = stdenv.mkDerivation {
    name = "networkmanager-hook";
    src = ./__init__.py;
    buildInputs = [ python3 ];
    nativeBuildInputs = [ makeWrapper python3.pkgs.mypy ];
    dontUnpack = true;
    installPhase = ''
      install -D -m755 $src $out/bin/dispatcher
      patchShebangs $out/bin/dispatcher
      mypy $src
      wrapProgram $out/bin/dispatcher --prefix PATH : ${lib.makeBinPath [systemd alsa-utils]}
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
    dispatcherScripts = [
      {
        source = "${networkmanager-hook}/bin/dispatcher";
      }
    ];

    plugins = with pkgs; lib.mkForce [
      #networkmanager-fortisslvpn
      #networkmanager-iodine
      #networkmanager-l2tp
      #networkmanager-openconnect
      networkmanager-openvpn
      networkmanager-vpnc
      #networkmanager-sstp
    ];

  };

  # Use mac address from thinkpad network dongle also for thinkpad docking
  # station and screen, since we will not need more than two 1 Gigabyte uplinks
  systemd.network.links."00-docking-station".extraConfig = ''
    [Match]
    MACAddress = 08:3a:88:59:80:71 f4:6b:8c:4a:81:c0

    [Link]
    MACAddress = 8c:8c:aa:da:9d:35
    Name = dock0
  '';
}
