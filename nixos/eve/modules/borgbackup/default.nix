{ pkgs, config, ... }:
{
  sops.secrets.borg-passphrase = {};
  sops.secrets.borg-ssh = {};
  sops.secrets.nas-wakeup-password = {};
  sops.secrets.healthcheck-borgbackup = {};

  services.borgbackup.repos.uni = {
    path = "/data/backup/uni";
    authorizedKeys = [ (builtins.readFile ./uni-borgbackup.pub) ];
  };

  services.borgbackup.repos.turingmachine = {
    path = "/data/backup/turingmachine";
    authorizedKeys = [ (builtins.readFile ./turingmachine-borgbackup.pub) ];
  };

  services.borgbackup.jobs.eve = {
    paths = [
      "/home"
      "/etc"
      "/var"
      "/root"
    ];
    repo = "eve-backup@home.devkid.net:backup";
    encryption = {
      mode = "repokey";
      passCommand = "cat ${config.sops.secrets.borg-passphrase.path}";
    };
    compression = "auto,zstd";
    startAt = "daily";
    environment.BORG_RSH = "ssh -oPort=22022";
    preHook = ''
      set -x
      eval $(ssh-agent)
      ssh-add ${config.sops.secrets.borg-ssh.path}
      ${pkgs.netcat}/bin/nc -w20 home.devkid.net 22198 < ${config.sops.secrets.nas-wakeup-password.path}
      for i in $(seq 1 20); do
        if ${pkgs.netcat}/bin/nc -z -v -w1 home.devkid.net 22022; then
          break
        fi
        sleep 1
      done
    '';

    postHook = ''
      token=$(cat ${config.sops.secrets.healthcheck-borgbackup.path})
      if [[ "$exitStatus" == "0" ]]; then
        ${pkgs.curl}/bin/curl -XPOST -fsS --retry 3 https://hc-ping.com/$token
      else
        ${pkgs.curl}/bin/curl -XPOST -fsS --retry 3 https://hc-ping.com/$token/fail
      fi
    '';

    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 7;
      weekly = 4;
      monthly = 0;
    };
  };
}
