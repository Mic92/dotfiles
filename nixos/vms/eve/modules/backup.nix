{ config, lib, pkgs, ... }:

with builtins;

{
  systemd.timers.backup = {
    wantedBy = ["multi-user.target"];
    timerConfig.OnCalendar = "02:07:00";
  };

  deployment.keys = {
    "mysql-password".keyFile = ../secrets/mysql-password;
    "nas-wakeup-password".keyFile = ../secrets/nas-wakeup-password;
  };

  systemd.services.backup = {
    path = with pkgs; [ bash netcat (pkgs.callPackage ../pkgs/backup.nix {}) ];
    script = ''
      set -eu -o pipefail
      cat /run/keys/nas-wakeup-password | nc -w1 -v 172.23.75.65 22198
      backup-container ${../lxc/container.json}
    '';
  };
}
