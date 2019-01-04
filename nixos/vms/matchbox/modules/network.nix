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

      [DHCP]
      UseHostname=false
      RouteMetric=512
    '';
  };

  networking.retiolum = {
    ipv4 = "10.243.29.176";
    ipv6 = "42:0:3c46:6745:adf4:a844:26c4:bf91";
  };
}
