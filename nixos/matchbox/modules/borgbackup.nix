{ pkgs, config, ... }: {
  fileSystems."/mnt/backup" = {
    device = "//192.168.178.1/FRITZ.NAS/TOSHIBA-ExternalUSB3-0-02";
    fsType = "cifs";
    options = let
      automount_opts = "noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=10s,x-systemd.mount-timeout=10s,soft,vers=1.0";
      # cat > smb-secrets <<EOF
      # username=s16916XX
      # domain=ED
      # password=<EASE_PASSWORD>
      # EOF
    in [
      "${automount_opts},credentials=${config.sops.secrets.smb-secrets.path}"
    ];
  };

  services.borgbackup.jobs.matchbox = {
    repo = "/mnt/backup/borg";
    paths = [
      "/home"
      "/etc"
      "/var"
      "/root"
      "/mnt/hdd/public/Dorit"
      "/mnt/hdd/public/falk"
      "/mnt/hdd/public/Daniela"
      "/mnt/hdd/public/Bilder"
      "/mnt/hdd/public/Joerg"
    ];
    encryption.mode = "none";
    removableDevice = true;
    postHook = ''
      cat > /var/log/telegraf/borgbackup-matchbox <<EOF
      task,frequency=weekly last_run=$(date +%s)i,state="$([[ $exitStatus == 0 ]] && echo ok || echo fail)"
      EOF
    '';
    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 7;
      weekly = 4;
      monthly = 3;
    };
  };

  systemd.services.borgbackup-job-matchbox.serviceConfig.ReadWritePaths = [
    "/var/log/telegraf"
  ];

  sops.secrets.smb-secrets = {};
}
