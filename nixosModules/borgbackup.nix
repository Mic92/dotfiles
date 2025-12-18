{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ ./borgbackup-zfs-snapshots.nix ];

  clan.core.state = {
    networkmanager = lib.mkIf (config.networking.networkmanager.enable) {
      folders = [ "/etc/NetworkManager" ];
    };
    system.folders = [
      "/home"
      "/var"
      "/root"
    ];
  };

  # The borgbackup configuration is now handled by the clan inventory system
  # This module only defines the state folders to be backed up
  services.borgbackup.jobs = {
    blob64 = {
      repo = lib.mkForce "borg@blob64.x:/zdata/borg/${config.networking.hostName}";
      preHook = lib.optionalString config.networking.networkmanager.enable ''
        # wait until network is available and not metered
        while ! ${pkgs.networkmanager}/bin/nm-online --quiet || ${pkgs.networkmanager}/bin/nmcli --terse --fields GENERAL.METERED dev show 2>/dev/null | grep --quiet "yes"; do
          sleep 60
        done
      '';
      postHook = ''
        cat > /var/log/telegraf/borgbackup-job-${config.networking.hostName}.service <<EOF
        task,frequency=daily last_run=$(date +%s)i,state="$([[ $exitStatus == 0 ]] && echo ok || echo fail)"
        EOF
      '';
      exclude = [
        "*.pyc"
        "*.o"
        "*/node_modules/*"
        "/home/*/go/"
        "/home/*/.direnv"
        "/home/*/.cache"
        "/home/*/.cargo"
        "/home/*/.npm"
        "/home/*/.m2"
        "/home/*/.gradle"
        "/home/*/.opam"
        "/home/*/.clangd"
        "/home/*/.config/Ferdium/Partitions"
        "/home/*/.mozilla/firefox/*/storage"
        "/home/*/Android"
        "/var/lib/containerd"
        # already included in database backup
        "/var/lib/postgresql"
        "/var/lib/docker/"
        "/var/log/journal"
        "/var/lib/containerd"
        "/var/lib/systemd" # not so interesting state so far
        "/var/lib/private/dendrite/searchindex"
        "/var/cache"
        "/var/tmp"
        "/var/log"

        "/home/joerg/sync"
        "/home/joerg/Videos"
        "/home/joerg/mnt"
      ];
    };
  };

  systemd.services."borgbackup-job-blob64".serviceConfig.ReadWritePaths = [ "/var/log/telegraf" ];
}
