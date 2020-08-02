{
  systemd.network.networks."retiolum".extraConfig = ''
    [Network]
    Address=fd42:4492:6a6d:500:f610:15d1:27a3:674b/128

    [Route]
    Destination=fd42:4492:6a6d:500::/64
    Metric=1200
  '';

  networking.retiolum = {
    ipv4 = "10.243.29.168";
    ipv6 = "42:0:3c46:47e8:f610:15d1:27a3:674b";
  };
}
