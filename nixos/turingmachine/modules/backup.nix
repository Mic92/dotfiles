{ config, lib, pkgs, ... }:
with builtins;

let
  backupPath = "/mnt/backup/borg";
in {
  systemd.timers.backup = {
    wantedBy = ["multi-user.target"];
    timerConfig.OnCalendar = "12:00:00";
  };

  systemd.services.borgbackup-job-turingmachine.unitConfig.RequiresMountsFor = backupPath;

  sops.secrets.borgbackup = {};
  sops.secrets.healthcheck-borgbackup = {};

  services.borgbackup.jobs.turingmachine = {
    removableDevice = true;
    paths = [
      "/home"
      "/etc"
      "/var"
      "/root"
    ];
    repo = backupPath;
    exclude = [
      "/home/*/.cache"
      "/home/*/.config/Ferdi/Partitions"
    ];
    encryption = {
      mode = "repokey";
      passCommand = "cat ${config.sops.secrets.borgbackup.path}";
    };
    preHook = ''
      set -x
      # Could be dangerous, but works.
      # In case an backup was aborted....
      borg break-lock "${backupPath}"
    '';
    postHook = ''
      token=$(cat ${config.sops.secrets.healthcheck-borgbackup.path})
      if [[ "$exitStatus" == "0" ]]; then
        ${pkgs.curl}/bin/curl -XPOST -fsS --retry 3 https://hc-ping.com/$token
      else
        ${pkgs.curl}/bin/curl -XPOST -fsS --retry 3 https://hc-ping.com/$token/fail
      fi
    '';

    postPrune = ''
      ${pkgs.utillinux}/bin/umount -l /mnt/backup || true
    '';

    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 7;
      weekly = 4;
      monthly = 3;
    };
  };
  environment.systemPackages = with pkgs; [ borgbackup ];
  sops.secrets.healthcheck-borgbackup-turingmachine = {};
}
