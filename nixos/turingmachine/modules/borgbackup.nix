{ config
, lib
, pkgs
, ...
}:
let
  backupPath = "borg@blob64.r:/zdata/borg/turingmachine";
in
{
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
      "/home/*/.emacs.d"
      "/home/*/.cache"
      "/home/*/.cargo"
      "/home/*/.npm"
      "/home/*/.m2"
      "/home/*/.gradle"
      "/home/*/.opam"
      "/home/*/.clangd"
      "/home/*/Android"
      "/home/*/.config/Ferdi/Partitions"
      "/home/*/.mozilla/firefox/*/storage"
      "/home/joerg/Musik/podcasts"
      "/home/joerg/gPodder/Downloads"
      "/home/joerg/sync"
      "/home/joerg/Videos"
      "/home/joerg/git/linux/*.qcow2"
      "/home/joerg/git/OSX-KVM/mac_hdd_ng.img"
      "/home/joerg/mnt"
      "/var/lib/containerd"
      "/var/log/journal"
      "/var/cache"
      "/var/tmp"
      "/var/log"
    ];
    environment.BORG_RSH = "ssh -i ${config.sops.secrets.turingmachine-ssh-borgbackup.path}";
    encryption = {
      mode = "repokey";
      passCommand = "cat ${config.sops.secrets.turingmachine-borgbackup.path}";
    };
    postHook = ''
      cat > /var/log/telegraf/borgbackup-job-turingmachine.service <<EOF
      task,frequency=daily last_run=$(date +%s)i,state="$([[ $exitStatus == 0 ]] && echo ok || echo fail)"
      EOF
    '';

    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 7;
      weekly = 4;
      monthly = 3;
    };
  };

  systemd.services.break-borgbackup-lock = {
    path = [ pkgs.borgbackup pkgs.openssh ];
    script = ''
      eval $(ssh-agent)
      ssh-add ${config.sops.secrets.turingmachine-ssh-borgbackup.path}
      export BORG_PASSCOMMAND='cat /run/secrets/borgbackup'
      export BORG_REPO='${backupPath}'
      borg break-lock
    '';
  };

  systemd.timers.borgbackup-job-turingmachine = {
    timerConfig.OnCalendar = lib.mkForce "12:00:00";
  };

  systemd.services.borgbackup-job-turingmachine.serviceConfig.ReadWritePaths = [
    "/var/log/telegraf"
  ];
}
