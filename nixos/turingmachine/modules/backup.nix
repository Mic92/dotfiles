{ config, lib, pkgs, ... }:
with builtins;

let
  backupPath = "borgbackup@eddie.r:turingmachine/borg";
in {
  systemd.timers.backup = {
    wantedBy = ["multi-user.target"];
    timerConfig.OnCalendar = "12:00:00";
  };

  sops.secrets.borgbackup = {};
  sops.secrets.ssh-borgbackup = {};
  sops.secrets.healthcheck-borgbackup = {};

  services.borgbackup.jobs.turingmachine = {
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
      "*.pyc"
      "/home/*/.direnv"
      "/home/*/.cache"
      "/home/*/.cargo"
      "/home/*/.npm"
      "/home/*/.m2"
      "/home/*/.gradle"
      "/home/*/.opam"
      "/home/*/.clangd"
      "/home/*/Android"
      "/home/*/.config/Ferdi/Partitions"
      "/home/joerg/Musik/podcasts"
      "/home/joerg/gPodder/Downloads"
      "/home/joerg/sync"
      "/home/joerg/git/OSX-KVM/mac_hdd_ng.img"
      "/home/joerg/mnt"
      "/var/lib/docker"
      "/var/log/journal"
      "/var/cache"
      "/var/tmp"
    ];
    encryption = {
      mode = "repokey";
      passCommand = "cat ${config.sops.secrets.borgbackup.path}";
    };
    #${pkgs.sshfs}/bin/sshfs \
    #  -oworkaround=rename \
    #  -oIdentityFile=${config.sops.secrets.ssh-borgbackup.path} \
    #  -oProxyJump=sshjump@eddie.r \
    #  -oPort=22222 \
    #  s1691654@csce.datastore.ed.ac.uk:/csce/datastore/inf/users/s1691654 \
    #  /mnt/backup
    preHook = ''
      set -x
      mkdir -p /mnt/backup
      eval $(ssh-agent)
      ssh-add ${config.sops.secrets.ssh-borgbackup.path}
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
