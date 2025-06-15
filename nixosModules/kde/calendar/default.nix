{ pkgs, ... }:

let
  calendarNotifyScript = pkgs.writeShellScriptBin "calendar-notify" ''
    #!/usr/bin/env bash
    # Check for upcoming calendar events and send KDE notifications

    STATE_DIR="$HOME/.local/state/calendar-notify"
    mkdir -p "$STATE_DIR"
    STATE_FILE="$STATE_DIR/notified-events"

    # Clean up old entries (older than 24 hours)
    if [[ -f "$STATE_FILE" ]]; then
      TEMP_FILE=$(mktemp)
      NOW=$(date +%s)
      while IFS='|' read -r timestamp event_id; do
        if (( NOW - timestamp < 86400 )); then
          echo "$timestamp|$event_id" >> "$TEMP_FILE"
        fi
      done < "$STATE_FILE"
      mv "$TEMP_FILE" "$STATE_FILE"
    fi

    # Get events for the next hour with full details
    EVENTS=$(${pkgs.khal}/bin/khal list --format "{start}|{title}|{uid}" now 1h 2>/dev/null)

    if [[ -n "$EVENTS" ]]; then
      while IFS= read -r event; do
        if [[ -n "$event" ]]; then
          # Parse event details
          IFS='|' read -r DATETIME TITLE EVENT_UID <<< "$event"
          TIME=$(echo "$DATETIME" | cut -d' ' -f2)
          DATE=$(echo "$DATETIME" | cut -d' ' -f1)
          
          # Create unique event ID
          EVENT_ID="$DATE $TIME $EVENT_UID"
          
          # Check if already notified
          if ! grep -qF "|$EVENT_ID" "$STATE_FILE" 2>/dev/null; then
            # Send notification
            ${pkgs.libnotify}/bin/notify-send \
              --urgency=normal \
              --app-name="Calendar" \
              --icon=office-calendar \
              "Event at $TIME" \
              "$TITLE"
            
            # Record notification
            echo "$(date +%s)|$EVENT_ID" >> "$STATE_FILE"
          fi
        fi
      done <<< "$EVENTS"
    fi
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
