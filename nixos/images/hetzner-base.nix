{
  imports = [
    ./base-config.nix
    ./zfs.nix
  ];

  networking.dhcpcd.enable = false;

  systemd.network = {
    enable = true;
    networks."eth0".extraConfig = ''
      [Match]
      Name = eth0

      [Network]
      Address = 95.216.112.61/26
      Gateway = 95.216.112.1
      Address = 2a01:4f9:2b:1605::2/64
      Gateway = fe80::1
      IPv6AcceptRA = no
      IPForward = yes

      [DHCP]
      UseDNS = no
    '';
  };

  networking.nameservers = [
    "213.133.98.98"
    "213.133.99.99"
    "213.133.100.100"
    "2a01:4f8:0:1::add:1010"
    "2a01:4f8:0:1::add:9999"
    "2a01:4f8:0:1::add:9898"
  ];
}
