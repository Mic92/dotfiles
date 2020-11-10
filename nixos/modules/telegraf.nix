{ pkgs, lib, config, ... }: let
  isVM = lib.any (mod: mod == "xen-blkfront" || mod == "virtio_console") config.boot.initrd.kernelModules;
in {
  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 9273 ];
  services.telegraf = {
    enable = true;
    extraConfig = {
      inputs = {
        smart = lib.mkIf (!isVM) {
          path = pkgs.writeShellScript "smartctl" ''
            exec /run/wrappers/bin/sudo ${pkgs.smartmontools}/bin/smartctl "$@"
          '';
        };
        system = {};
        mem = {};
        file = {
          name_override = "ext4_errors";
          files = [ "/sys/fs/ext4/*/errors_count" ];
          data_format = "value";
        };
        exec = lib.optionalAttrs (lib.any (fs: fs == "zfs") config.boot.supportedFilesystems) {
          ## Commands array
          commands = [
            (pkgs.writeScript "zpool-health" ''
              #!${pkgs.gawk}/bin/awk -f
              BEGIN {
                  while ("${pkgs.zfs}/bin/zpool status" | getline) {
                      if ($1 ~ /pool:/) { printf "zpool_status,name=%s ", $2 }
                      if ($1 ~ /state:/) { printf " state=\"%s\",", $2 }
                      if ($1 ~ /errors:/) {
                          if (index($2, "No")) printf "errors=0i\n"; else printf "errors=%di\n", $2
                      }
                  }
              }
            '')
          ];
          data_format = "influx";
        };
        systemd_units = {};
        swap = {};
        disk.tagdrop = {
          fstype = [ "tmpfs" "ramfs" "devtmpfs" "devfs" "iso9660" "overlay" "aufs" "squashfs" ];
          device = [ "rpc_pipefs" "lxcfs" "nsfs" "borgfs" ];
        };
      };
      outputs = {
        prometheus_client = {
          listen = ":9273";
          metric_version = 2;
        };
      };
    };
  };
  security.sudo.extraRules = lib.mkIf (!isVM) [{
    users = [ "telegraf" ];
    commands = [ {
      command = "${pkgs.smartmontools}/bin/smartctl";
      options = [ "NOPASSWD" ];
    }];
  }];
  # avoid logging sudo use
  security.sudo.configFile = ''
    Defaults:telegraf !syslog,!pam_session
  '';
}
