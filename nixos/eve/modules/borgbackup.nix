{ pkgs, config, ... }:
{
  sops.secrets.borg-passphrase = {};
  sops.secrets.nas-wakeup-password = {};
  sops.secrets.healthcheck-borgbackup = {};

  services.borgbackup.jobs.eve = {
    paths = [
      "/home"
      "/etc"
      "/var"
      "/root"
    ];
    repo = "eve-backup@backup:backup";
    encryption = {
      mode = "repokey";
      passCommand = "cat ${config.sops.secrets.borg-passphrase.path}";
    };
    compression = "auto,zstd";
    startAt = "daily";
    preHook = ''
      set -x
      ${pkgs.netcat}/bin/nc -w20 home.devkid.net 22198 < ${config.sops.secrets.nas-wakeup-password.path}
      for i in $(seq 1 20); do
        if ${pkgs.netcat}/bin/nc -z -v -w1 home.devkid.net 22022; then
          break
        fi
        sleep 1
      done
    '';

    postHook = ''
      ${pkgs.nur.repos.mic92.healthcheck}/bin/healthcheck \
        --service borgbackup --failed $exitStatus \
        --password-file ${config.sops.secrets.healthcheck-borgbackup.path}
    '';

    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 7;
      weekly = 4;
      monthly = 0;
    };
  };
}
