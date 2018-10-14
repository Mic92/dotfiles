{ config, lib, ... }: {
  services.netdata = {
    enable = true;
    config = {
      global = {
        "bind to" = (lib.concatStringsSep " " [
          "127.0.0.1:19999"
          "${config.networking.retiolum.ipv4}:19999"
          "[${config.networking.retiolum.ipv6}]:19999"
        ]);
        "error log" = "stderr";
      };
    };
  };
  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 19999 ];

  # TODO create /etc/netdata
}
