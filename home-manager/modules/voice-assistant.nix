{
  self,
  pkgs,
  ...
}:
let
  packages = self.packages.${pkgs.stdenv.hostPlatform.system};
  # noctalia, systemctl, loginctl and sh come from the installed profiles.
  path = "PATH=/etc/profiles/per-user/%u/bin:/run/current-system/sw/bin";
in
{
  # va-eval, va-reload and va-check, to poke at the live brain.
  home.packages = [ packages.voice-assistant-brain ];

  # Its own service, so it can be reloaded or restarted without the
  # assistant losing the microphone.
  systemd.user.services.voice-assistant-brain = {
    Unit = {
      Description = "Voice assistant brain";
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      # Ready once the socket listens, so the assistant never starts its own.
      Type = "notify";
      NotifyAccess = "all";
      ExecStart = "${packages.voice-assistant-brain}/bin/va-brain";
      Environment = [ path ];
      Restart = "on-failure";
      RestartSec = 2;
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  systemd.user.services.voice-assistant = {
    Unit = {
      Description = "Wake word voice assistant";
      After = [
        "graphical-session.target"
        "pipewire.service"
        "voice-assistant-brain.service"
      ];
      Wants = [ "voice-assistant-brain.service" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${packages.voice-assistant}/bin/voice-assistant";
      Environment = [ path ];
      Restart = "on-failure";
      RestartSec = 5;
      Nice = 5;
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
