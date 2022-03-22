let
  ip4 = "192.168.21.2";
  ip6 = "2a01:4f8:10b:49f:1::2";
in {
  services.nginx.virtualHosts."beherbergung.thalheim.io" = {
    enableACME = true;
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://[${ip6}]:3000;
    '';
  };

  services.nginx.virtualHosts."backend.beherbergung.thalheim.io" = {
    enableACME = true;
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://[${ip6}]:4000;
    '';
  };

  systemd.nspawn.joe02 = {
    enable = true;
    execConfig = {
      Capability = "all";
      PrivateUsers = "no";
    };
    networkConfig.Bridge = "br-joe";
  };
  networking.nat = {
    forwardPorts = [
      {
        sourcePort = 2202;
        loopbackIPs = [
          "192.168.21.254"
        ];
        destination = "${ip4}:22";
      }
      {
        sourcePort = 2202;
        destination = "[${ip6}]:22";
      }
    ];
  };
}
