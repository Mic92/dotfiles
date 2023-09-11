{ pkgs, ... }: {
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
          cat > /var/log/telegraf/borgbackup-matchbox.service <<EOF
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
