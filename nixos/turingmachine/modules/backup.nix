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

  services.borgbackup.jobs.turingmachine = {
    removableDevice = true;
    paths = [
      "/home"
      "/etc"
      "/var"
      "/root"
    ];
    repo = backupPath;
    exclude = [ "/home/*/.cache" ];
    encryption = {
      mode = "repokey";
      passCommand = "cat ${toString <secrets/borgbackup>}";
    };
    preHook = ''
      # Could be dangerous, but works.
      # In case an backup was aborted....
      borg break-lock "${backupPath}"
    '';
    postHook = ''
      ${pkgs.nur.repos.mic92.healthcheck}/bin/healthcheck \
        --service borgbackup-turingmachine --failed $exitStatus \
        --password-file ${toString <secrets/healthcheck-borgbackup-turingmachine>}
    '';

    postCreate = ''
      ${pkgs.utillinux}/bin/umount -l /mnt/backup
    '';

    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 7;
      weekly = 4;
      monthly = 3;
    };
  };
  environment.systemPackages = with pkgs; [ borgbackup ];
}
