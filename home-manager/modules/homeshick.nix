{ pkgs, lib, ... }:
let
  homeshickPath = "\${HOMESHICK_DIR:-$HOME/.homesick/repos/homeshick}/bin/homeshick";
in
{
  systemd.user.services.homeshick-symlink = lib.mkIf pkgs.stdenv.isLinux {
    Unit = {
      Description = "Homeshick symlink dotfiles";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.bash}/bin/bash -c '${homeshickPath} symlink dotfiles --batch'";
      RemainAfterExit = true;
      Environment = [
        "HOME=%h"
        "PATH=${
          lib.makeBinPath [
            pkgs.coreutils
            pkgs.gitMinimal
            pkgs.bash
          ]
        }"
      ];
      WorkingDirectory = "%h";
    };

    Install = {
      WantedBy = [ "default.target" ];
    };
  };

  launchd.agents.homeshick-symlink = lib.mkIf pkgs.stdenv.isDarwin {
    enable = true;
    config = {
      ProgramArguments = [
        "${pkgs.bash}/bin/bash"
        "-c"
        "${homeshickPath} symlink dotfiles --batch"
      ];
      RunAtLoad = true;
      Label = "org.homeshick.symlink";
      EnvironmentVariables = {
        HOME = "/Users/%u";
        PATH = "${lib.makeBinPath [
          pkgs.coreutils
          pkgs.gitMinimal
          pkgs.bash
        ]}";
      };
      WorkingDirectory = "/Users/%u";
    };
  };
}
