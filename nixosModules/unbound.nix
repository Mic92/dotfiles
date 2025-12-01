{ ... }:
{
  services.unbound = {
    enable = true;
    settings = {
      server = {
        #verbosity = 3;
        prefetch = "yes";
        prefetch-key = true;
        qname-minimisation = true;
        do-not-query-localhost = false;
        # Too many broken dnssec setups even at big companies such as amazon.
        # Breaks my email setup. Better rely on tls for security.
        val-permissive-mode = "yes";
      };
      remote-control.control-enable = true;
    };
  };

  # Since we use this for local dns resolving, we don't want to stop/start but
  # just restart, so we quickly get it back.
  systemd.services.unbound.stopIfChanged = false;
}
