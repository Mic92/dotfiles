{ pkgs, config, lib, ...}: {
  options.tailscale.derper.hostname = lib.mkOption {
    type = lib.types.str;
    description = "hostname for cert";
  };
  config = {
    networking.domain = "thalheim.io";
    services.tailscale.enable = true;
    # this port is open in university so lets use it.
    services.tailscale.port = 51820;
    networking.firewall.allowedUDPPorts = [
      51820 # wireguard
    ];

    networking.firewall.checkReversePath = "loose";

  };
}
