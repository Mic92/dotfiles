{ ... }: {
  systemd.network.networks = {
    ethernet.extraConfig = ''
      [Match]
      Name = eth0

      [Network]
      DHCP=both
      LLMNR=true
      IPv4LL=true
      LLDP=true
      IPv6AcceptRA=true
      IPForward=ipv6
      IPv6ProxyNDPAddress=fd42:4492:6a6d:500:f610:15d1:27a3:674b
      IPv6ProxyNDPAddress=fd42:4492:6a6d:500:8526:2adf:7451:8bbb

      [DHCP]
      UseHostname=false
      RouteMetric=512
    '';
    "retiolum".extraConfig = ''
      [Network]
      IPForward=ipv6

      [Route]
      Destination=fd42:4492:6a6d:500:f610:15d1:27a3:674b

      [Route]
      Destination=fd42:4492:6a6d:500:8526:2adf:7451:8bbb
    '';
  };

  networking.networkmanager.dns = "systemd-resolved";

  networking.retiolum = {
    ipv4 = "10.243.29.176";
    ipv6 = "42:0:3c46:6745:adf4:a844:26c4:bf91";
  };
}
