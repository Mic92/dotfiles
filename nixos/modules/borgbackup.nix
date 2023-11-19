{ config, ... }: {
  systemd.services."borgbackup-job-${config.networking.hostName}".serviceConfig.ReadWritePaths = [
    "/var/log/telegraf"
  ];

  services.borgbackup.jobs.${config.networking.hostName} = {
    paths = [
      "/home"
      "/etc"
      "/var"
      "/root"
    ];
    exclude = [
      "*.pyc"
      "/home/*/.direnv"
      "/home/*/.cache"
      "/home/*/.npm"
      "/home/*/.m2"
      "/home/*/.gradle"
      "/home/*/.opam"
      "/home/*/.clangd"
      "/home/*/.config/Ferdium/Partitions"
      "/home/*/.mozilla/firefox/*/storage"
      "/home/*/Android"
      "/var/lib/containerd"
      # already included in database backup
      "/var/lib/postgresql"
      "/var/lib/docker/"
      "/var/log/journal"
      "/var/lib/containerd"
      "/var/lib/systemd" # not so interesting state so far
      "/var/lib/private/dendrite/searchindex"
      "/var/cache"
      "/var/tmp"
      "/var/log"

      "/home/joerg/sync"
      "/home/joerg/Videos"
      "/home/joerg/mnt"
      # eve
      "/home/joerg/work/kuutamo/core/src/kuutamod/.data"
    ];
    repo = "borg@blob64.r:/zdata/borg/${config.networking.hostName}";
    encryption = {
      mode = "repokey";
      passCommand = "cat ${config.sops.secrets."${config.networking.hostName}-borgbackup-passphrase".path}";
    };
    environment.BORG_RSH = "ssh -i ${config.sops.secrets."${config.networking.hostName}-borgbackup-ssh".path}";
    compression = "auto,zstd";
    startAt = "daily";
    preHook = "set -x";

    postHook = ''
      cat > /var/log/telegraf/borgbackup-job-${config.networking.hostName}.service <<EOF
      task,frequency=daily last_run=$(date +%s)i,state="$([[ $exitStatus == 0 ]] && echo ok || echo fail)"
      EOF
    '';

    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 7;
      weekly = 4;
      monthly = 0;
    };
  };
}
