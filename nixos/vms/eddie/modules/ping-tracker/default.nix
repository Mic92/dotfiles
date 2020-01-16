{ pkgs, ... }: {
  systemd.services.ping-tracker = {
    serviceConfig = {
      Environment = "PATH=/run/wrappers/bin";
      ExecStart = "${pkgs.python3.interpreter} ${./ping_tracker.py} /run/keys/ping-tracker.json";
      Type = "oneshot";
      SupplementaryGroups = [ "keys" ];
      User = "ping-tracker";
    };
  };

  systemd.timers.ping-tracker = {
    description = "Update home-assistant location status";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnUnitActiveSec = "5min";
      OnBootSec = "5min";
    };
  };

  users.users.ping-tracker.group = "ping-tracker";
  users.groups.ping-tracker = {};

  krops.secrets.files."ping-tracker.json".owner = "ping-tracker";

  security.wrappers.fping.source = "${pkgs.fping}/bin/fping";
}
