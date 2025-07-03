{
  config,
  lib,
  pkgs,
  ...
}:

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
    ${lib.optionalString pkgs.stdenv.isLinux ''export PATH="${pkgs.libnotify}/bin:$PATH"''}
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
  config = {
    home.packages = with pkgs; [
      khal
      vdirsyncer
      todoman
      calendarNotifyScript
      calendarSyncScript
    ];

    # Platform-specific service configuration
    systemd.user = lib.mkIf pkgs.stdenv.isLinux {
      services.calendar-sync = {
        Unit = {
          Description = "Sync calendars with vdirsyncer";
          After = [ "network-online.target" ];
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${calendarSyncScript}/bin/calendar-sync";
        };
      };

      services.calendar-notify = {
        Unit = {
          Description = "Check calendar and send notifications";
          After = [ "graphical-session.target" ];
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${calendarNotifyScript}/bin/calendar-notify";
          Environment = "DISPLAY=:0";
        };
      };

      timers.calendar-sync = {
        Unit = {
          Description = "Sync calendars regularly";
        };
        Timer = {
          OnCalendar = "*:0/15";
          Persistent = true;
        };
        Install = {
          WantedBy = [ "timers.target" ];
        };
      };

      timers.calendar-notify = {
        Unit = {
          Description = "Check calendar for upcoming events";
        };
        Timer = {
          OnCalendar = "*:0/5";
          Persistent = true;
        };
        Install = {
          WantedBy = [ "timers.target" ];
        };
      };
    };

    launchd.agents = lib.mkIf pkgs.stdenv.isDarwin {
      calendar-sync = {
        enable = true;
        config = {
          ProgramArguments = [ "${calendarSyncScript}/bin/calendar-sync" ];
          StartCalendarInterval = [
            {
              Minute = 0;
            }
            {
              Minute = 15;
            }
            {
              Minute = 30;
            }
            {
              Minute = 45;
            }
          ];
          StandardOutPath = "${config.home.homeDirectory}/.local/state/calendar-sync.log";
          StandardErrorPath = "${config.home.homeDirectory}/.local/state/calendar-sync.err";
        };
      };

      calendar-notify = {
        enable = true;
        config = {
          ProgramArguments = [ "${calendarNotifyScript}/bin/calendar-notify" ];
          StartCalendarInterval = [
            {
              Minute = 0;
            }
            {
              Minute = 5;
            }
            {
              Minute = 10;
            }
            {
              Minute = 15;
            }
            {
              Minute = 20;
            }
            {
              Minute = 25;
            }
            {
              Minute = 30;
            }
            {
              Minute = 35;
            }
            {
              Minute = 40;
            }
            {
              Minute = 45;
            }
            {
              Minute = 50;
            }
            {
              Minute = 55;
            }
          ];
          StandardOutPath = "${config.home.homeDirectory}/.local/state/calendar-notify.log";
          StandardErrorPath = "${config.home.homeDirectory}/.local/state/calendar-notify.err";
        };
      };
    };
  };
}
