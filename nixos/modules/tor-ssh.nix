{...}: {
  services.tor = {
    enable = true;
    relay.onionServices."ssh".map = [
      {
        port = 22;
      }
    ];
    settings = {
      DnsPort = 9053;
      AutomapHostsOnResolve = true;
      AutomapHostsSuffixes = [".exit" ".onion"];
      EnforceDistinctSubnets = true;
      ExitNodes = "{de}";
      EntryNodes = "{de}";
      NewCircuitPeriod = 120;
      DNSPort = 9053;
    };
  };

  imports = [./sshd.nix];
}
