{ pkgs, ... }:

{
  services.borgbackup.jobs.eve = {
    paths = [
      "/home"
      "/etc"
      "/srv"
      "/var"
      "/usr/local"
      "/var"
      "/opt"
    ];
    repo = "eve-backup@backup:backup";
    encryption = {
      mode = "repokey";
      passCommand = "cat /run/keys/borg-passphrase";
    };
    compression = "auto,zstd";
    startAt = "daily";
    preHook = ''
      ${pkgs.netcat}/bin/nc -w3 home.devkid.net 22198 < /run/keys/nas-wakeup-password
    '';
    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 7;
      weekly = 4;
      monthly = 0;
    };
  };

  deployment.keys = {
    "nas-wakeup-password".keyFile = ../secrets/nas-wakeup-password;
    "borg-passphrase".keyFile = ../secrets/borg-passphrase;
  };
}
