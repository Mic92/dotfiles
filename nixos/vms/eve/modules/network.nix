{
  networking.dhcpcd.enable = false;

  systemd.network = {
    enable = true;
    networks."eth0".extraConfig = ''
      [Match]
      Name = eth0

      [Network]
      DHCP = ipv4
      Address = 2a03:4000:13:31e::1/128
      Address = 2a03:4000:13:31e:1::10/128
      Address = 2a03:4000:13:31e:1::5/128
      Address = 2a03:4000:13:31e:1::6/128
      Gateway = fe80::1
      IPv6AcceptRA = no
      IPForward = yes

      [DHCP]
      UseDNS = no
    '';
  };

  networking.firewall.enable = true;
  networking.nat = {
    enable = true;
    externalInterface = "eth0";
    internalIPs = [ "192.168.92.0/24" ];
  };
}
