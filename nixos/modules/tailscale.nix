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
    networking.firewall.allowedUDPPorts = [ 51820 ];

    networking.firewall.checkReversePath = "loose";

    systemd.services.derper = {
      after = ["network.target"];
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        StateDirectory = "derper";
        ExecStart = "${pkgs.tailscale}/bin/derper --certdir /var/lib/derper --hostname=${config.networking.fqdn} --verify-clients";
      };
    };
  };
}
