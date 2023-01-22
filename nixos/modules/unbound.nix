{
  services.unbound = {
    enable = true;
    settings.server = {
      prefetch = "yes";
      prefetch-key = true;
      qname-minimisation = true;
    };
  };

  # Since we use this for local dns resolving, we don't want to stop/start but
  # just restart, so we quickly get it back.
  systemd.services.unbound.stopIfChanged = false;
}
