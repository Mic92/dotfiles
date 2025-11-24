{ pkgs, lib, ... }:
with pkgs;
let
  networkmanager-hook = stdenv.mkDerivation {
    name = "networkmanager-hook";
    src = ./__init__.py;
    buildInputs = [ python3 ];
    nativeBuildInputs = [
      makeWrapper
      python3.pkgs.mypy
    ];
    dontUnpack = true;
    installPhase = ''
      install -D -m755 $src $out/bin/dispatcher
      patchShebangs $out/bin/dispatcher
      mypy $src
      wrapProgram $out/bin/dispatcher --prefix PATH : ${
        lib.makeBinPath [
          systemd
          alsa-utils
        ]
      }
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
    dispatcherScripts = [ { source = "${networkmanager-hook}/bin/dispatcher"; } ];

    plugins =
      with pkgs;
      lib.mkForce [
        networkmanager-openvpn
        networkmanager-vpnc
      ];
  };

  # Use mac address from thinkpad network dongle also for thinkpad docking
  # station and screen, since we will not need more than two 1 Gigabyte uplinks
  systemd.network.links."00-docking-station".extraConfig = ''
    [Match]
    MACAddress = 00:e0:4c:a3:e1:8c 08:3a:88:59:80:71 f4:6b:8c:4a:81:c0 6c:1f:f7:02:91:64 f4:a8:0d:05:55:fd 8c:8c:aa:da:9d:35


    [Link]
    MACAddress = 8c:8c:aa:da:9d:35
    Name = dock0
  '';

  # Configure USB-C ethernet adapter with DHCP server
  systemd.network.networks."10-usbc-ethernet" = {
    matchConfig.Name = "enp195s0f4u1c2";
    address = [ "192.168.42.1/24" ];
    networkConfig = {
      DHCPServer = true;
      IPv6PrivacyExtensions = "yes";
    };
    dhcpServerConfig = {
      PoolOffset = 100;
      PoolSize = 100;
      EmitDNS = true;
      DNS = [ "192.168.42.1" ];
    };
  };

  # Make interface unmanaged by NetworkManager
  networking.networkmanager.unmanaged = [ "enp195s0f4u1c2" ];

  # Allow DHCP server traffic
  networking.firewall.allowedUDPPorts = [ 67 ];
}
