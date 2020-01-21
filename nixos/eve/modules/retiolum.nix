{
  imports = [
    ../../modules/retiolum.nix
  ];
  systemd.network.networks."retiolum".extraConfig = ''
    [Network]
    Address=fd42:4492:6a6d:500:8526:2adf:7451:8bbb/64
  '';

  networking.retiolum = {
    ipv4 = "10.243.29.174";
    ipv6 = "42:0:3c46:70c7:8526:2adf:7451:8bbb";
  };

  services.icinga2.extraConfig = ''
    apply Service "Tinc v4 (eve)" {
      import "eve-tcp4-service"
      vars.tcp_port = 655
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "Tinc v6 (eve)" {
      import "eve-tcp6-service"
      vars.tcp_port = 655
      assign where host.name == "eve.thalheim.io"
    }
  '';
}
