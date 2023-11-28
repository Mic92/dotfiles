{ config
, lib
, ...
}:
let
  external = "wlp0s20f3";
in
{
  systemd.network.netdevs.internal.netdevConfig = {
    Name = "internal";
    Kind = "bridge";
  };
  systemd.network.networks = {
    internal.extraConfig = ''
      [Match]
      Name=internal

      [Network]
      Address=192.168.32.50/24
      LLMNR=true
      LLDP=true
    '';
  };

  # Add any internal interface with the following command:
  # $ nmcli dev disconnect eth0
  # $ ip link set eth0 master internal
  services.dnsmasq.enable = !config.virtualisation.libvirtd.enable;
  services.dnsmasq.resolveLocalQueries = lib.mkDefault false; # don't use it locally for dns
  services.dnsmasq.settings = {
    interface = "internal";
    #interface=virttap
    listen-address = "127.0.0.1";
    dhcp-range = "192.168.32.50,192.168.32.100,12h";
    # disable dns
    port = 0;
    # no gateway and no dns
    dhcp-option = [ 3 6 ];
    # static leases
    dhcp-host = [
      "52:54:00:1d:e1:33,192.168.32.2"
      "52:54:00:8f:73:d7,192.168.32.3"
    ];
  };

  #''
  #'';

  networking.nat = {
    enable = true;
    externalInterface = external;
    internalInterfaces = [ "internal" ];
  };

  networking.firewall.allowedTCPPorts = [
    # pixiecore
    64172
  ];
  networking.firewall.allowedUDPPorts = [
    # pixiecore
    69
    4011
    # dnsmasq
    53
    67
  ];
}
