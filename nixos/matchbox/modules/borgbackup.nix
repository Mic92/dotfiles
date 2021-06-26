{ pkgs, lib, config, ... }: {
  fileSystems."/mnt/backup" = {
    device = "UUID=11ac8bec-aef1-45ca-a530-2115d403ce53";
    fsType = "ext4";
    options = [ "noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=10s,x-systemd.mount-timeout=10s" ];
  };

  services.rsnapshot = {
    enable = true;
    enableManualRsnapshot = true;
    extraConfig = ''
      # test
      no_create_root	1
      snapshot_root	/mnt/backup/rsnapshot
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
  #services.borgbackup.jobs.matchbox = {
  #  repo = "/mnt/backup/borg";
  #  paths = [
  #    "/home"
  #    "/etc"
  #    "/var"
  #    "/root"
  #    "/mnt/hdd/public/Dorit"
  #    "/mnt/hdd/public/falk"
  #    "/mnt/hdd/public/Daniela"
  #    "/mnt/hdd/public/Bilder"
  #    "/mnt/hdd/public/Joerg"
  #  ];
  #  encryption.mode = "none";
  #  removableDevice = true;
  #  postHook = ''
  #    cat > /var/log/telegraf/borgbackup-matchbox <<EOF
  #    task,frequency=weekly last_run=$(date +%s)i,state="$([[ $exitStatus == 0 ]] && echo ok || echo fail)"
  #    EOF
  #  '';
  #  prune.keep = {
  #    within = "1d"; # Keep all archives from the last day
  #    daily = 7;
  #    weekly = 4;
  #    monthly = 3;
  #  };
  #};

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
