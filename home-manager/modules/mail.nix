{
  config,
  pkgs,
  lib,
  self,
  ...
}:

let
  # email-sync script from flake packages
  email-sync = self.packages.${pkgs.stdenv.hostPlatform.system}.email-sync;

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
lib.mkMerge [
  {
    home.packages = with pkgs; [
      # Email sync
      isync # mbsync
      notmuch
      email-sync # our email sync script

      aerc

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

      self.packages.${pkgs.stdenv.hostPlatform.system}.vcal
    ];

    # All config files are managed by homeshick:
    # - .mbsyncrc
    # - .notmuch-config
    # - .config/aerc/accounts.conf
    # - .config/aerc/aerc.conf
    # - .config/aerc/notmuch-querymap
    # - bin/email-sync

    # notmuch post-new hook is managed by homeshick in home/.notmuch/hooks/
  }
  # Email sync automation - use systemd on Linux, launchd on macOS
  (lib.mkIf pkgs.stdenv.isLinux {
    # Systemd timer for email sync (Linux only)
    systemd.user.services.mbsync = {
      Unit = {
        Description = "Mailbox synchronization";
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${email-sync}/bin/email-sync";
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
  })

  (lib.mkIf pkgs.stdenv.isDarwin {
    # Launchd agent for email sync (macOS only)
    launchd.enable = true;
    launchd.agents.mbsync = {
      enable = true;
      config = {
        ProgramArguments = [
          "${email-sync}/bin/email-sync"
        ];
        StartInterval = 300; # 5 minutes in seconds
        RunAtLoad = true;
        StandardOutPath = "${config.home.homeDirectory}/.local/state/mbsync.log";
        StandardErrorPath = "${config.home.homeDirectory}/.local/state/mbsync.err";
        EnvironmentVariables = {
          HOME = config.home.homeDirectory;
        };
      };
    };
  })
]
