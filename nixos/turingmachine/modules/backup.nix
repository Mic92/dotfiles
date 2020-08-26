{ config, lib, pkgs, ... }:
with builtins;

let
  backupPath = "/mnt/backup/borg";
in {
  systemd.timers.backup = {
    wantedBy = ["multi-user.target"];
    timerConfig.OnCalendar = "12:00:00";
  };

  sops.secrets.borgbackup = {};
  sops.secrets.ssh-borgbackup = {};
  sops.secrets.healthcheck-borgbackup = {};

  services.borgbackup.jobs.turingmachine = {
    removableDevice = true;
    paths = [
      "/home"
      "/etc"
      "/var"
      "/root"
    ];
    # runs borg list, which is really slow over sshfs
    doInit = false;
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
      mkdir -p /mnt/backup
      eval $(ssh-agent)
      ssh-add ${config.sops.secrets.ssh-borgbackup.path}
      ${pkgs.sshfs}/bin/sshfs \
        -oIdentityFile=${config.sops.secrets.ssh-borgbackup.path} \
        -oProxyJump=sshjump@eddie.r \
        -oPort=22222 \
        s1691654@csce.datastore.ed.ac.uk:/csce/datastore/inf/users/s1691654 \
        /mnt/backup \
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

    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 7;
      weekly = 4;
      monthly = 3;
    };
  };

  systemd.services.borgbackup-job-turingmachine.serviceConfig.PrivateMounts = true;

  environment.systemPackages = with pkgs; [ borgbackup ];
  sops.secrets.healthcheck-borgbackup = {};
}
