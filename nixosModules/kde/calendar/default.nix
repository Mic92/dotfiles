{ pkgs, ... }:

let
  pythonEnv = pkgs.python3.withPackages (
    ps: with ps; [
      icalendar
      python-dateutil
      pytz
    ]
  );

  calendarNotifyScript = pkgs.writeShellScriptBin "calendar-notify" ''
    #!/usr/bin/env bash
    export PATH="${pkgs.libnotify}/bin:$PATH"
    exec ${pythonEnv}/bin/python ${./calendar-notify.py}
  '';

  calendarSyncScript = pkgs.writeShellScriptBin "calendar-sync" ''
    #!/usr/bin/env bash
    # Sync calendars with vdirsyncer
    export PATH="${pkgs.rbw}/bin:$PATH"
    ${pkgs.vdirsyncer}/bin/vdirsyncer sync
  '';

in
{
  environment.systemPackages = with pkgs; [
    khal
    vdirsyncer
    todoman
    calendarNotifyScript
    calendarSyncScript
  ];

  # Enable systemd user services at NixOS level
  systemd.user.services.calendar-sync = {
    description = "Sync calendars with vdirsyncer";
    after = [ "network-online.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${calendarSyncScript}/bin/calendar-sync";
    };
  };

  systemd.user.services.calendar-notify = {
    description = "Check calendar and send notifications";
    after = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${calendarNotifyScript}/bin/calendar-notify";
    };
    environment = {
      DISPLAY = ":0";
    };
  };

  systemd.user.timers.calendar-sync = {
    description = "Sync calendars regularly";
    timerConfig = {
      OnCalendar = "*:0/15";
      Persistent = true;
    };
    wantedBy = [ "timers.target" ];
  };

  systemd.user.timers.calendar-notify = {
    description = "Check calendar for upcoming events";
    timerConfig = {
      OnCalendar = "*:0/5";
      Persistent = true;
    };
    wantedBy = [ "timers.target" ];
  };
}
