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

  systemd.services.backup = let
    backup = pkgs.callPackage ../pkgs/backup.nix {};
  in {
    serviceConfig.ExecStart = "${backup}/bin/backup-container ${../lxc/container.json}";
  };
}
