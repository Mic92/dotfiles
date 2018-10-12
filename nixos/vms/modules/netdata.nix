{ config, ... }: {
  services.netdata = {
    enable = true;
    config = {
      global = {
        "bind to" = "127.0.0.1:19999 ${config.networking.retiolum.ipv4}:19999 [${config.networking.retiolum.ipv6}]:19999";
      };
    };
  };
  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 19999 ];
}
