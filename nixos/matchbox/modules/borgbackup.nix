{ config, pkgs, ... }: {

  sops.secrets.borgbackup-passphrase = { };
  sops.secrets.borgbackup-ssh = { };

  systemd.services.borgbackup-job-blob64.serviceConfig.ReadWritePaths = [
    "/var/log/telegraf"
  ];

  services.borgbackup.jobs.blob64 = {
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
      "/var/lib/containerd"
      "/mnt/hdd"
      # already included in database backup
      "/var/lib/postgresql"
      "/var/lib/docker/"
      "/var/log/journal"
      "/var/cache"
      "/var/tmp"
      "/var/log"
    ];
    repo = "borg@blob64.r:/zdata/borg/matchbox";
    encryption = {
      mode = "repokey";
      passCommand = "cat ${config.sops.secrets.borgbackup-passphrase.path}";
    };
    compression = "auto,zstd";
    startAt = "daily";
    doInit = true;
    preHook = ''
      set -x
      eval $(ssh-agent)
      ssh-add ${config.sops.secrets.borgbackup-ssh.path}
    '';

    postHook = ''
      cat > /var/log/telegraf/borgbackup-matchbox <<EOF
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

  fileSystems."/mnt/backup" = {
    device = "UUID=11ac8bec-aef1-45ca-a530-2115d403ce53";
    fsType = "ext4";
    options = [ "noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=10s,x-systemd.mount-timeout=10s" ];
  };

  systemd.tmpfiles.rules = [
    "d /mnt/backup 0755 root root -"
  ];

  services.rsnapshot = {
    enable = true;
    enableManualRsnapshot = true;
    extraConfig = ''
      snapshot_root	/mnt/backup/rsnapshot
      cmd_preexec	${
        pkgs.writeShellScript "mount" ''
          set -eux -o pipefail
          if ! ${pkgs.util-linux}/bin/mountpoint -q /mnt/backup; then
            ${pkgs.util-linux}/bin/mount /mnt/backup
          fi
        ''
      }
      cmd_postexec	${
        pkgs.writeShellScript "umount" ''
          set -eux -o pipefail
          cat > /var/log/telegraf/borgbackup-matchbox <<EOF
          task,frequency=weekly last_run=$(date +%s)i,state="ok"
          EOF
          ${pkgs.util-linux}/bin/umount /mnt/backup
        ''
      }
      retain	daily	30
      retain	monthly	3
      backup	/home	matchbox/
      backup	/etc	matchbox/
      backup	/var	matchbox/
      backup	/root	matchbox/
      backup	/mnt/hdd/public/Dorit	matchbox/
      backup	/mnt/hdd/public/falk	matchbox/
      backup	/mnt/hdd/public/Daniela	matchbox/
      backup	/mnt/hdd/public/Bilder	matchbox/
      backup	/mnt/hdd/public/Joerg	matchbox/
    '';
    cronIntervals = {
      monthly = "0 2 1 * *";
      daily = "0 5 * * *";
    };
  };
}
