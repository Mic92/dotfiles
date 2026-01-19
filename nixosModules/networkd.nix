{
  systemd.network.enable = true;

  services.resolved.settings.Resolve = {
    #DNSOverTLS = "yes";
    # docker
    DNSStubListenerExtra = "172.17.0.1";
    # dns.thalheim.io performs dnssec already
    DNSSEC = "false";
  };

  # don't take down the network for too long
  systemd.services.systemd-networkd.stopIfChanged = false;
  # Services that are only restarted might be not able to resolve when this is stopped before
  systemd.services.systemd-resolved.stopIfChanged = false;

  networking.firewall.allowedUDPPorts = [ 5353 ];

  networking.dhcpcd.enable = false;
}
