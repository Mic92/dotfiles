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
}
