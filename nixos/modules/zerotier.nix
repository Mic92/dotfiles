{ lib, pkgs, ... }:
{
  networking.firewall.allowedTCPPorts = [ 9993 ];
  networking.firewall.allowedUDPPorts = [ 9993 ];
  networking.firewall.interfaces."zt+".allowedTCPPorts = [ 5353 ];
  networking.firewall.interfaces."zt+".allowedUDPPorts = [ 5353 ];

  services.zerotierone = {
    enable = true;
    joinNetworks = [
      "33d87fa6bd93423e"
    ];
  };

  systemd.network.networks.zerotier.extraConfig = ''
    [Match]
    Name=zt*

    [Network]
    LLMNR=true
    LLDP=true
    MulticastDNS=true
    KeepConfiguration=static
  '';

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "zerotierone"
  ];

  systemd.tmpfiles.rules = [
    "L+ /var/lib/zerotier-one/local.conf - - - - ${pkgs.writeText "local.conf" (builtins.toJSON {
      physical = {
        "10.243.0.0/16".blacklist = true;
        "10.244.0.0/16".blacklist = true;
        "10.250.0.0/16".blacklist = true;
        "42::/16".blacklist = true;
      };
      # virtual = {
      #   feedbeef12 = {
      #     role = "UPSTREAM";
      #     try = [ "10.10.20.1/9993" ];
      #     blacklist = [ "192.168.0.0/24" ];
      #   }
      # };
    })}"
  ];

  networking.networkmanager.unmanaged = [ "interface-name:zt*" ];
}
