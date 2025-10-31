{
  config,
  pkgs,
  lib,
  ...
}:

lib.mkMerge [
  {
    home.packages = [
      pkgs.atuin
    ];
  }

  # Systemd timer for atuin sync (Linux only)
  (lib.mkIf pkgs.stdenv.isLinux {
    systemd.user.timers.atuin-sync = {
      Unit.Description = "Atuin auto sync";
      Timer.OnUnitActiveSec = "1h";
      Install.WantedBy = [ "timers.target" ];
    };

    systemd.user.services.atuin-sync = {
      Unit.Description = "Atuin auto sync";

      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.atuin}/bin/atuin sync";
        IOSchedulingClass = "idle";
      };
    };
  })

  # Launchd agent for atuin sync (macOS only)
  (lib.mkIf pkgs.stdenv.isDarwin {
    launchd.enable = true;
    launchd.agents.atuin-sync = {
      enable = true;
      config = {
        ProgramArguments = [
          "${pkgs.atuin}/bin/atuin"
          "sync"
        ];
        StartInterval = 3600; # 1 hour in seconds
        RunAtLoad = true;
        ProcessType = "Background";
        StandardOutPath = "${config.home.homeDirectory}/.local/state/atuin-sync.log";
        StandardErrorPath = "${config.home.homeDirectory}/.local/state/atuin-sync.err";
      };
    };
  })
]
