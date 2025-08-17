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
      ExecStart = "${pkgs.bash}/bin/bash -c '${homeshickPath} symlink dotfiles --force'";
      RemainAfterExit = true;
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
        "${homeshickPath} symlink dotfiles --force"
      ];
      RunAtLoad = true;
      Label = "org.homeshick.symlink";
    };
  };
}
