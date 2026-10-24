{
  self,
  pkgs,
  ...
}:
{
  systemd.user.services.voice-assistant = {
    Unit = {
      Description = "Wake word voice assistant";
      After = [
        "graphical-session.target"
        "pipewire.service"
      ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${
        self.packages.${pkgs.stdenv.hostPlatform.system}.voice-assistant
      }/bin/voice-assistant";
      # noctalia, systemctl, loginctl and sh come from the installed profiles.
      Environment = [ "PATH=/etc/profiles/per-user/%u/bin:/run/current-system/sw/bin" ];
      Restart = "on-failure";
      RestartSec = 5;
      Nice = 5;
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
