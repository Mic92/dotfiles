{ config, pkgs, ... }:

let
  # aerc with compose pipe support
  aerc-patched = pkgs.callPackage ../../pkgs/aerc-patched.nix { 
    inherit (pkgs) aerc;
  };
  # msmtp wrapper that saves sent mail to maildir
  msmtp-with-sent = pkgs.writeShellScriptBin "msmtp" ''
    # Wrapper for msmtp that saves sent mail to maildir

    # Create temp file for the email
    tmpfile=$(mktemp)
    trap "rm -f $tmpfile" EXIT

    # Read email from stdin and save to temp file
    cat > "$tmpfile"

    # Send the email with real msmtp
    if ${pkgs.msmtp}/bin/msmtp "$@" < "$tmpfile"; then
        # If send successful, save to Sent folder
        # Generate unique filename for maildir
        timestamp=$(date +%s)
        hostname=$(hostname)
        pid=$$
        random=$RANDOM
        filename="''${timestamp}.''${pid}_''${random}.''${hostname}:2,S"

        # Ensure Sent directory exists
        mkdir -p "$HOME/mail/thalheim.io/.Sent/cur"

        # Save to Sent/cur directory with Seen flag
        cp "$tmpfile" "$HOME/mail/thalheim.io/.Sent/cur/$filename"

        # Update notmuch database
        ${pkgs.notmuch}/bin/notmuch new >/dev/null 2>&1

        exit 0
    else
        # If send failed, exit with msmtp's exit code
        exit $?
    fi
  '';
in
{
  home.packages = with pkgs; [
    # Email sync
    isync # mbsync
    notmuch

    # CLI email clients
    aerc-patched
    neomutt # alternative to aerc

    # Email organization tools
    afew # notmuch tagging tool
    notmuch-addrlookup
    mblaze # maildir manipulation tools

    # Contacts management
    khard # CardDAV address book

    # Password management for email
    rbw # Bitwarden CLI (already used in your mbsync config)

    # Optional tools
    msmtp-with-sent # msmtp wrapper that saves to Sent folder
    w3m # for HTML email viewing
  ];

  # All config files are managed by homeshick:
  # - .mbsyncrc
  # - .notmuch-config
  # - .config/aerc/accounts.conf
  # - .config/aerc/aerc.conf
  # - .config/aerc/notmuch-querymap
  # - bin/email-sync

  # Systemd timer for email sync (optional)
  systemd.user.services.mbsync = {
    Unit = {
      Description = "Mailbox synchronization";
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${config.home.homeDirectory}/.homesick/repos/dotfiles/home/bin/email-sync";
    };
  };

  systemd.user.timers.mbsync = {
    Unit = {
      Description = "Mailbox synchronization timer";
    };
    Timer = {
      OnBootSec = "2m";
      OnUnitActiveSec = "5m";
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
  };

  # notmuch post-new hook is managed by homeshick in home/.notmuch/hooks/
}
