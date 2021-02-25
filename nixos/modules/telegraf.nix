{ pkgs, lib, config, ... }: let
  isVM = lib.any (mod: mod == "xen-blkfront" || mod == "virtio_console") config.boot.initrd.kernelModules;
in {
  options = {
    mic92.telegraf.mode = lib.mkOption {
      type = lib.types.enum [ "push" "pull"];
      default = "pull";
      description = ''
        Wether to pull/push metrics to prometheus
      '';
    };
  };
  config = {
    networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 9273 ];

    systemd.services.telegraf.path = [ pkgs.nvme-cli ];

    services.telegraf = {
      enable = true;
      environmentFiles = lib.optional (config.mic92.telegraf.mode == "push")
        config.sops.secrets.telegraf-shared.path;
      extraConfig = {
        agent.interval = "60s";
        inputs = {
          kernel_vmstat = {};
          smart = lib.mkIf (!isVM) {
            path = pkgs.writeShellScript "smartctl" ''
              exec /run/wrappers/bin/sudo ${pkgs.smartmontools}/bin/smartctl "$@"
            '';
          };
          system = {};
          mem = {};
          file = [{
            data_format = "influx";
            file_tag = "name";
            files = [ "/var/log/telegraf/*" ];
          }] ++ lib.optional (lib.any (fs: fs == "ext4") config.boot.supportedFilesystems) {
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
        outputs = if config.mic92.telegraf.mode == "pull" then {
          prometheus_client = {
            listen = ":9273";
            metric_version = 2;
          };
        } else {
          influxdb_v2 = {
            urls = [ "https://telegraf.thalheim.io" ];
            token = ''''${INFLUXDB_PASSWORD}'';
            insecure_skip_verify = false;
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
    # create dummy file to avoid telegraf errors
    systemd.tmpfiles.rules = [
      "f /var/log/telegraf/dummy 0444 root root - -"
    ];
    sops.secrets = lib.mkIf (config.mic92.telegraf.mode == "push") {
      telegraf-shared = {
        owner = config.systemd.services.telegraf.serviceConfig.User;
        sopsFile = ../secrets/telegraf.yaml;
      };
    };
  };
}
