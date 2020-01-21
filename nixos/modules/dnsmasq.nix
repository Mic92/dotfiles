{ ... }: {
  services.dnsmasq.enable = true;
  services.dnsmasq.extraConfig = ''
    interface=enp0s25
    listen-address=127.0.0.1
    dhcp-range=192.168.32.50,192.168.32.100,12h
    ## no gateway
    #dhcp-option=3
    ## no dns
    #dhcp-option=6
    ## static leases
    #dhcp-host=52:54:00:1d:e1:33,192.168.32.2
    #dhcp-host=52:54:00:8f:73:d7,192.168.32.3
  '';
}
