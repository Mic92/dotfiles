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
    startAt = "Mon,Fri *-*-* 00:00:00";
    removableDevice = true;
    preHook = ''
      set -x
      hc_token=$(cat ${config.sops.secrets.healthcheck-borgbackup.path})
      ${pkgs.curl}/bin/curl -XPOST -fsS --retry 3 https://hc-ping.com/$hc_token/start
    '';
    postHook = ''
      if [[ "$exitStatus" == "0" ]]; then
        ${pkgs.curl}/bin/curl -XPOST -fsS --retry 3 https://hc-ping.com/$hc_token
      else
        ${pkgs.curl}/bin/curl -XPOST -fsS --retry 3 https://hc-ping.com/$hc_token/fail
      fi
    '';
    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 7;
      weekly = 4;
      monthly = 3;
    };
  };

  sops.secrets.smb-secrets = {};
  sops.secrets.healthcheck-borgbackup = {};
}
