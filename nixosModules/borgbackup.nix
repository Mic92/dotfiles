{
  config,
  lib,
  pkgs,
  ...
}:
let
  jobModule =
    { name, ... }:
    {
      preHook = lib.optionalString config.networking.networkmanager.enable ''
        # wait until network is available and not metered
        while ! ${pkgs.networkmanager}/bin/nm-online --quiet || ${pkgs.networkmanager}/bin/nmcli --terse --fields GENERAL.METERED dev show 2>/dev/null | grep --quiet "yes"; do
          sleep 60
        done
      '';
      postHook = ''
        cat > /var/log/telegraf/borgbackup-job-${config.networking.hostName}${
          lib.optionalString (name != "blob64") "-${name}"
        }.service <<EOF
        task,frequency=daily last_run=$(date +%s)i,exit_status=''${exitStatus}i
        EOF
      '';
      extraArgs = lib.mkIf (name == "storagebox") "--remote-path=borg-1.4";
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
        "/var/lib/systemd"
        "/var/cache"
        "/var/tmp"
        "/var/log"

        "/home/joerg/sync"
        "/home/joerg/Videos"
        "/home/joerg/mnt"
      ];
    };
  jobs = builtins.attrNames config.services.borgbackup.jobs;
in
{
  imports = [ ./borgbackup-zfs-snapshots.nix ];

  options.services.borgbackup.jobs = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule jobModule);
  };

  config = {
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

    services.borgbackup.jobs.blob64.repo =
      lib.mkForce "borg@blob64.x:/zdata/borg/${config.networking.hostName}";

    systemd.services = lib.genAttrs (map (n: "borgbackup-job-${n}") jobs) (_: {
      serviceConfig.ReadWritePaths = [ "/var/log/telegraf" ];
    });
  };
}
