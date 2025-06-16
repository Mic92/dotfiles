{ pkgs, ... }:

let
  email-notify = pkgs.writeShellScriptBin "email-notify" ''
    set -euo pipefail

    # Query for new emails in inbox folders from the last 7 days
    # that haven't been notified yet
    NEW_EMAILS=$(${pkgs.notmuch}/bin/notmuch search --output=messages '(folder:thalheim.io OR folder:Entwickler OR folder:Netzwerke OR folder:Benachrichtigung OR folder:Geld OR folder:Privat OR folder:Arbeit OR folder:Uni) AND date:7d.. AND NOT tag:notified AND NOT tag:trash' 2>/dev/null || true)

    if [ -z "$NEW_EMAILS" ]; then
        exit 0
    fi

    # Process each new email
    while IFS= read -r msg_id; do
        # Get email details
        FROM=$(${pkgs.notmuch}/bin/notmuch show --format=json "$msg_id" | ${pkgs.jq}/bin/jq -r '.[0][0][0].headers.From' | sed 's/<.*>//g' | xargs)
        SUBJECT=$(${pkgs.notmuch}/bin/notmuch show --format=json "$msg_id" | ${pkgs.jq}/bin/jq -r '.[0][0][0].headers.Subject')
        
        # Send desktop notification
        ${pkgs.libnotify}/bin/notify-send "New Email" "$FROM: $SUBJECT" --icon=mail-unread
        
        # Mark as notified so we don't notify again
        ${pkgs.notmuch}/bin/notmuch tag +notified -- "$msg_id"
    done <<< "$NEW_EMAILS"
  '';
in
{
  systemd.user.services.email-notifications = {
    Unit = {
      Description = "Email notifications for inbox messages";
      After = [ "graphical-session.target" ];
    };

    Service = {
      Type = "oneshot";
      ExecStart = "${email-notify}/bin/email-notify";
    };
  };

  systemd.user.timers.email-notifications = {
    Unit = {
      Description = "Check for new emails every minute";
    };

    Timer = {
      OnBootSec = "1min";
      OnUnitActiveSec = "1min";
    };

    Install = {
      WantedBy = [ "timers.target" ];
    };
  };
}
