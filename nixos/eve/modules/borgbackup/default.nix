{ pkgs, config, ... }:
{
  sops.secrets.borg-passphrase = { };
  sops.secrets.borg-nas-ssh = { };
  sops.secrets.nas-wakeup-password = { };

  services.borgbackup.repos.uni = {
    path = "/data/backup/uni";
    authorizedKeys = [ (builtins.readFile ./uni-borgbackup.pub) ];
  };

  services.borgbackup.repos.turingmachine = {
    path = "/data/backup/turingmachine";
    authorizedKeys = [ (builtins.readFile ./turingmachine-borgbackup.pub) ];
  };

  systemd.services.borgbackup-job-hetzner.serviceConfig.ReadWritePaths = [
    "/var/log/telegraf"
  ];

  services.borgbackup.jobs.hetzner = {
    paths = [
      "/home"
      "/etc"
      "/var"
      "/root"
    ];
    repo = "u242570@u242570.your-storagebox.de:/./borg";
    encryption = {
      mode = "repokey";
      passCommand = "cat ${config.sops.secrets.borg-passphrase.path}";
    };
    compression = "auto,zstd";
    startAt = "daily";
    environment.BORG_RSH = "ssh -oPort=23";
    preHook = ''
      set -x
      eval $(ssh-agent)
      ssh-add ${config.sops.secrets.borg-nas-ssh.path}
    '';

    postHook = ''
      cat > /var/log/telegraf/borgbackup-eve <<EOF
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
}
