{ config, ... }: {
  sops.secrets.borgbackup-passphrase = { };
  sops.secrets.borgbackup-ssh = { };

  systemd.services.borgbackup-job-eva.serviceConfig.ReadWritePaths = [
    "/var/log/telegraf"
  ];

  services.borgbackup.jobs.eva = {
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
      "/home/*/.cargo"
      "/home/*/.npm"
      "/home/*/.m2"
      "/home/*/.gradle"
      "/home/*/.opam"
      "/home/*/.clangd"
      "/home/*/.config/Ferdi/Partitions"
      "/home/*/.mozilla/firefox/*/storage"
      "/home/joerg/work/kuutamo/core/src/kuutamod/.data"
      "/var/lib/containerd"
      # already included in database backup
      "/var/lib/postgresql"
      # not so important
      "/var/db/influxdb"
      "/var/lib/docker/"
      "/var/log/journal"
      "/var/cache"
      "/var/tmp"
      "/var/log"
    ];
    repo = "borg@blob64.r:/zdata/borg/eve";
    encryption = {
      mode = "repokey";
      passCommand = "cat ${config.sops.secrets.borgbackup-passphrase.path}";
    };
    compression = "auto,zstd";
    startAt = "daily";
    preHook = ''
      set -x
      eval $(ssh-agent)
      ssh-add ${config.sops.secrets.borgbackup-ssh.path}
    '';

    postHook = ''
      cat > /var/log/telegraf/borgbackup-eve <<EOF
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
