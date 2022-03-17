let
  ip4 = "192.168.21.1";
  ip6 = "2a01:4f8:10b:49f:1::1";
in {
  services.nginx.virtualHosts."search.warhelp.eu" = {
    enableACME = true;
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://[${ip6}]:3000;
    '';
  };

  services.nginx.virtualHosts."search.warhelp.broenradio.org" = {
    enableACME = true;
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://[${ip6}]:3000;
    '';
  };

  services.nginx.virtualHosts."backend.search.warhelp.eu" = {
    enableACME = true;
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://[${ip6}]:4000;
    '';
  };

  services.nginx.virtualHosts."backend.warhelp.broenradio.org" = {
    enableACME = true;
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://[${ip6}]:4000;
    '';
  };

  systemd.nspawn.joe01 = {
    enable = true;
    execConfig = {
      Capability = "all";
      PrivateUsers = "no";
    };
  };
  networking.nat = {
    forwardPorts = [
      {
        sourcePort = 2201;
        loopbackIPs = [
          "192.168.21.254"
        ];
        destination = "${ip4}:22";
      }
      {
        sourcePort = 2201;
        destination = "[${ip6}]:22";
      }
    ];
  };
}
