{ ... }: {
  services.tor = {
    enable = true;
    hiddenServices."ssh".map = [{
      port = 22;
    }];
    settings = {
      DnsPort = 9053;
      AutomapHostsOnResolve = 1;
      AutomapHostsSuffixes = ".exit,.onion";
      EnforceDistinctSubnets = 1;
      ExitNodes = "{de}";
      EntryNodes = "{de}";
      NewCircuitPeriod = 120;
      DNSPort = 9053:
    };
  };

  imports = [ ./sshd.nix ];
}
