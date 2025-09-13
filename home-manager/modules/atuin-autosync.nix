{ pkgs, ... }:
{
  home.packages = [
    pkgs.atuin
  ];
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
}
