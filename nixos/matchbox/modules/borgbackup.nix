{ pkgs, lib, config, ... }: {
  fileSystems."/mnt/backup" = {
    device = "UUID=11ac8bec-aef1-45ca-a530-2115d403ce53";
    fsType = "ext4";
    options = [ "noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=10s,x-systemd.mount-timeout=10s" ];
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
    preHook = "${pkgs.util-linux}/bin/mountpoint -q /mnt/backup || ${pkgs.util-linux}/bin/mount /mnt/backup";
    postHook = ''
      cat > /var/log/telegraf/borgbackup-matchbox <<EOF
      task,frequency=weekly last_run=$(date +%s)i,state="$([[ $exitStatus == 0 ]] && echo ok || echo fail)"
      EOF
      ${pkgs.util-linux}/bin/umount /mnt/backup
    '';
    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 7;
      weekly = 4;
      monthly = 3;
    };
  };

  #systemd.services.borgbackup-job-matchbox.serviceConfig.ReadWritePaths = [
  #  "/var/log/telegraf"
  #];

  # https://github.com/systemd/systemd/issues/17866
  systemd.services.borgbackup-job-matchbox.serviceConfig = {
    ProtectSystem = lib.mkForce false;
    ReadWritePaths = lib.mkForce (lib.mkAfter "");
  };

  sops.secrets.smb-secrets = { };
}
