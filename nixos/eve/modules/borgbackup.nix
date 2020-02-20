{ pkgs, ... }:
{
  services.borgbackup.jobs.eve = {
    paths = [
      "/home"
      "/etc"
      "/var"
    ];
    repo = "eve-backup@backup:backup";
    encryption = {
      mode = "repokey";
      passCommand = "cat /run/keys/borg-passphrase";
    };
    compression = "auto,zstd";
    startAt = "daily";
    preHook = ''
      set -x
      ${pkgs.netcat}/bin/nc -w20 home.devkid.net 22198 < /run/keys/nas-wakeup-password
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
        --password-file ${toString <secrets/healthcheck-borgbackup>}
    '';

    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 7;
      weekly = 4;
      monthly = 0;
    };
  };

  services.icinga2.healthchecks.borgbackup = {};

  krops.secrets.files.nas-wakeup-password = {};
  krops.secrets.files.borg-passphrase = {};
}
