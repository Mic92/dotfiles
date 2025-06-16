{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    # Email sync
    isync # mbsync
    notmuch

    # CLI email clients
    aerc
    neomutt # alternative to aerc

    # Email organization tools
    afew # notmuch tagging tool
    notmuch-addrlookup

    # Contacts management
    khard # CardDAV address book

    # Password management for email
    rbw # Bitwarden CLI (already used in your mbsync config)

    # Optional tools
    msmtp # for sending mail
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
