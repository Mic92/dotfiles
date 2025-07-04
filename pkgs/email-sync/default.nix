{
  lib,
  stdenv,
  writeShellApplication,
  isync,
  notmuch,
  afew,
  coreutils,
  gnugrep,
  jq,
  libnotify,
  khard,
  w3m,
  rbw,
  claude-code,
  terminal-notifier,
}:

writeShellApplication {
  name = "email-sync";

  runtimeInputs =
    [
      isync
      notmuch
      afew
      coreutils
      gnugrep
      jq
      khard
      w3m
      rbw
      claude-code
    ]
    ++ lib.optionals stdenv.isLinux [
      libnotify
    ]
    ++ lib.optionals stdenv.isDarwin [
      terminal-notifier
    ];

  text = ''
    # email-sync - Sync emails from IMAP and tag with afew

    set -euo pipefail

    # Sync emails from IMAP servers (folders already organized by Sieve)
    echo "Syncing emails from IMAP servers..."
    mbsync -a

    # Initialize notmuch if needed
    if [ ! -d "$HOME/mail/.notmuch" ]; then
        echo "Initializing notmuch database..."
        notmuch new
    fi

    # Index new emails
    echo "Indexing new emails..."
    notmuch new

    # The post-new hook will run afew automatically to tag emails
    # based on the filters in ~/.config/afew/config

    # Run afew to tag new emails (including verification codes)
    echo "Tagging emails with afew..."
    afew -tn

    # Run afew cleanup configuration on ALL emails
    echo "Running cleanup rules on all emails..."
    XDG_CONFIG_HOME=~/.config/afew-cleanup afew --tag --all

    # Move emails to appropriate folders based on MailMover rules
    echo "Moving emails to appropriate folders..."
    afew --move-mails --all

    # After moving emails, we might got rid of spam.
    mbsync thalheim:Spam thalheim:INBOX

    # Delete emails in trash older than 3 months
    echo "Deleting old trash emails (older than 3 months)..."
    TRASH_COUNT=$(notmuch count 'folder:Trash AND date:..3months')
    if [ "$TRASH_COUNT" -gt 0 ]; then
        echo "Deleting $TRASH_COUNT old trash emails..."
        notmuch search --output=files 'folder:Trash AND date:..3months' | xargs -r rm
        # Update notmuch database to remove deleted emails
        notmuch new
    fi

    # Send desktop notifications for new emails (if in graphical session)
    if [ -n "''${DISPLAY:-}" ] || [ -n "''${WAYLAND_DISPLAY:-}" ] || [[ "$OSTYPE" == "darwin"* ]]; then
        echo "Checking for new email notifications..."
        
        # Query for new emails in inbox folders from the last 7 days
        # that haven't been notified yet
        NEW_EMAILS=$(notmuch search --output=messages '(folder:thalheim.io OR folder:Entwickler OR folder:Netzwerke OR folder:Benachrichtigung OR folder:Geld OR folder:Privat OR folder:Arbeit OR folder:Uni) AND date:7d.. AND NOT tag:notified AND NOT tag:trash' 2>/dev/null || true)
        
        if [ -n "$NEW_EMAILS" ]; then
            # Process each new email
            while IFS= read -r msg_id; do
                # Get email details
                FROM=$(notmuch show --format=json "$msg_id" | jq -r '.[0][0][0].headers.From' | sed 's/<.*>//g' | xargs)
                SUBJECT=$(notmuch show --format=json "$msg_id" | jq -r '.[0][0][0].headers.Subject')
                
                # Send desktop notification based on OS
                if [[ "$OSTYPE" == "darwin"* ]]; then
                    # macOS notification using terminal-notifier
                    terminal-notifier -title "New Email" -message "$FROM: $SUBJECT"
                else
                    # Linux notification using notify-send
                    notify-send "New Email" "$FROM: $SUBJECT" --icon=mail-unread
                fi
                
                # Mark as notified so we don't notify again
                notmuch tag +notified -- "$msg_id"
            done <<< "$NEW_EMAILS"
        fi
    fi
  '';
}
