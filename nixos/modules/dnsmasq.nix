{ config, ... }:
let
  internal = "enp57s0u1";
  external = "wlan0";
in {
  #services.dnsmasq.enable = !config.virtualisation.libvirtd.enable;
  services.dnsmasq.enable = true;
  services.dnsmasq.extraConfig = ''
    interface=${internal}
    #interface=virttap
    listen-address=127.0.0.1
    dhcp-range=192.168.32.50,192.168.32.100,12h
    # disable dns
    port=0
    ## no gateway
    #dhcp-option=3
    ## no dns
    #dhcp-option=6
    ## static leases
    #dhcp-host=52:54:00:1d:e1:33,192.168.32.2
    #dhcp-host=52:54:00:8f:73:d7,192.168.32.3
  '';

  networking.nat = {
    enable = true;
    externalInterface = external;
    internalInterfaces = [ internal ];
  };

  networking.firewall.allowedTCPPorts = [
    # pixiecore
    64172
  ];
  networking.firewall.allowedUDPPorts = [
    # pixiecore
    69 4011
    # dnsmasq
    53 64
  ];
}
