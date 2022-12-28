{ config
, lib
, ...
}: {
  services.tor = {
    enable = true;
    client.enable = lib.mkDefault false;
    relay.onionServices."ssh".map = [
      {
        port = 22;
      }
    ];
    settings = {
      AutomapHostsOnResolve = true;
      AutomapHostsSuffixes = [ ".exit" ".onion" ];
      EnforceDistinctSubnets = true;
      UseEntryGuards = true;
      ExitNodes = "{de}";
      NewCircuitPeriod = 120;
      HiddenServiceNonAnonymousMode = !config.services.tor.client.enable;
      HiddenServiceSingleHopMode = !config.services.tor.client.enable;
    };
  };

  imports = [ ./. ];
}
