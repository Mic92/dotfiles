{
  config,
  pkgs,
  lib,
  self,
  ...
}:

let
  selfPkgs = self.packages.${pkgs.stdenv.hostPlatform.system};
  email-sync = selfPkgs.email-sync;
  msmtp-with-sent = selfPkgs.msmtp-with-sent;
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

      self.packages.${pkgs.stdenv.hostPlatform.system}.crabfit-cli
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
