{ config, pkgs, ... }: {
  imports = [
    ../../modules/networkd.nix
  ];

  networking.wireless.enable = true;

  systemd.network = {
    enable = true;
    networks."wlp2s0".extraConfig = ''
      [Match]
      Name = wlp2s0

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
