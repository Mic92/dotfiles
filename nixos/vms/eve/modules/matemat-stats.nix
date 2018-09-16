{ pkgs, ... }:
{
  systemd.timers = {
    "matemat-stats" = {
      wantedBy = ["multi-user.target"];
      timerConfig = {
        OnBootSec="20min";
        OnUnitActiveSec="60min";
      };
    };
  };

  systemd.services.matemat-stats = let
    matematEnv = pkgs.python3.buildEnv.override {
      extraLibs = with pkgs.python3Packages; [ requests influxdb ];
    };
  in {
    serviceConfig = {
      User = "nobody";
      ExecStart = "${matematEnv}/bin/python ${../scripts/matemat-stats.py}";
    };
  };
}
