{ config, ... }: {
  sops.secrets.borg-passphrase = { };
  sops.secrets.borg-nas-ssh = { };
  sops.secrets.ssh-borgbackup = { };
  sops.secrets.nas-wakeup-password = { };

  systemd.services.borgbackup-job-eva.serviceConfig.ReadWritePaths = [
    "/var/log/telegraf"
  ];

  fileSystems."/mnt/matchbox" = {
    device = "//matchbox.r/public";
    fsType = "cifs";
    options = [
      "guest"
      "nofail"
      "ro"
    ];
  };

  services.borgbackup.jobs.eva = {
    paths = [
      "/home"
      "/etc"
      "/var"
      "/root"
      "/mnt/matchbox"
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
      "/mnt/matchbox/ebooks"
      "/var/lib/containerd"
      # already included in database backup
      "/var/lib/postgresql"
      "/var/lib/docker/"
      "/var/log/journal"
      "/var/cache"
      "/var/tmp"
      "/var/log"
    ];
    repo = "borg@blob64.r:/zdata/borg/eve";
    encryption = {
      mode = "repokey";
      passCommand = "cat ${config.sops.secrets.borg-passphrase.path}";
    };
    compression = "auto,zstd";
    startAt = "daily";
    preHook = ''
      set -x
      eval $(ssh-agent)
      ssh-add ${config.sops.secrets.ssh-borgbackup.path}
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
