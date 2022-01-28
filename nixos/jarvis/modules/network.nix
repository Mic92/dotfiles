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
}
