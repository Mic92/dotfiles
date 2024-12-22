{ ... }:
{
  services.unbound = {
    enable = true;
    settings = {
      forward-zone = [
        {
          name = "\"hyprspace.\"";
          forward-addr = "127.43.104.80@11355";
        }
        # not implemented in hyprspace yet
        #{
        #  name = "\"0.0.0.0.0.0.0.0.0.0.5.6.3.6.1.6.0.7.3.7.2.7.0.7.9.7.8.6.0.0.d.f.ip6.arpa.\"";
        #  forward-addr = "127.43.104.80@11355";
        #}
        #{
        #  name = "\"64.100.in-addr.arpa.\"";
        #  forward-addr = "127.43.104.80@11355";
        #}
      ];
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
