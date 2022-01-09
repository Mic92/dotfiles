{ config, pkgs, ... }: {
  imports = [
    ../../modules/iwd.nix
    ../../modules/networkd.nix
  ];

  systemd.network = {
    enable = true;
    networks."wlan0".extraConfig = ''
      [Match]
      Name = wlan0

      [Network]
      DHCP = true
      IPv6AcceptRA = yes

      [IPv6AcceptRA]
      Token = ::fc0a:6bff:feb7:2b32

      [DHCP]
      UseDNS = no
    '';
  };

  networking.retiolum.ipv6 = "42:0:3c46:7338:dda8:2015:8d14:2a0b";
}
