{
  config,
  pkgs,
  self,
  lib,
  ...
}:

let
  alertmanager-bar = self.packages.${pkgs.stdenv.hostPlatform.system}.alertmanager-bar;
in
lib.mkMerge [
  {
    home.packages = [ alertmanager-bar ];
  }

  (lib.mkIf pkgs.stdenv.isDarwin {
    launchd.enable = true;
    launchd.agents.alertmanager-bar = {
      enable = true;
      config = {
        ProgramArguments = [
          "${alertmanager-bar}/bin/alertmanager-bar"
          "--url"
          "http://alertmanager.r"
          "--interval"
          "300"
        ];
        KeepAlive = true;
        RunAtLoad = true;
        ProcessType = "Background";
        StandardOutPath = "${config.home.homeDirectory}/.local/state/alertmanager-bar.log";
        StandardErrorPath = "${config.home.homeDirectory}/.local/state/alertmanager-bar.err";
      };
    };
  })
]
