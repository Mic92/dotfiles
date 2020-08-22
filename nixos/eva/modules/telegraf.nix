{ config, lib, pkgs, ... }:

{
  services.telegraf = {
    enable = true;
    extraConfig = {
      inputs = {
        ping = {
          urls = [
            "eddie.r"
            "eve.r"

            # cluster
            "rose.r"
            "martha.r"
            "donna.r"
            "amy.r"
            "clara.r"
          ];
          method = "native";
        };
      };
      outputs = {
        prometheus_client = {
          listen = ":9273";
          metric_version = 2;
        };
      };
    };
  };
}
