{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.netdata;
in
{
  options.services.netdata = {

    stream.role = mkOption {
      type = types.enum [ "master" "slave" ];
      default = "slave";
      description = "Wether to stream data";
    };

    httpcheck.checks = mkOption {
      type = types.attrsOf (types.submodule ({
        options = {
          url = mkOption {
            type = types.str;
            example = "https://thalheim.io";
            description = "Url to check";
          };
          regex = mkOption {
            type = types.nullOr types.str;
            default = null;
            example = "My homepage";
            description = "Regex that is matched against the returned content";
          };
          statusAccepted = mkOption {
            type = types.listOf types.int;
            default = [ 200 ];
            example = [ 401 ];
            description = "Expected http status code";
          };
        };
      }));
      default = {};
      description = ''
        httpcheck plugin: https://github.com/netdata/netdata/blob/master/collectors/python.d.plugin/httpcheck/httpcheck.conf
      '';
    };

    portcheck.checks = mkOption {
      type = types.attrsOf (types.submodule ({
        options = {
          host = mkOption {
            type = types.str;
            default = "127.0.0.1";
            description = "Dns name/IP to check";
          };
          port = mkOption {
            type = types.int;
            description = "Tcp port number";
          };
        };
      }));
      default = {};
      description = ''
        portcheck plugin: https://github.com/netdata/netdata/tree/master/collectors/python.d.plugin/portcheck
      '';
    };
  };
  config = {
    environment.etc."netdata/stream.conf".source = "/run/keys/netdata-stream.conf";

    deployment.keys."netdata-stream.conf" = {
      keyFile = if cfg.stream.role == "master" then
        ../../secrets/netdata-stream-master.conf
      else
        ../../secrets/netdata-stream-slave.conf;
      user = "netdata";
    };

    users.users.netdata.extraGroups = [ "keys" ];

    systemd.services.netdata.serviceConfig = {
      Environment="PYTHONPATH=${pkgs.netdata}/libexec/netdata/python.d/python_modules";
    };

    services.netdata = {
      enable = true;
      config = {
        global = {
          "bind to" = "0.0.0.0:19999 [::]:19999";
          "error log" = "stderr";
          "update every" = "5";
        };
        health.enable = if cfg.stream.role == "master" then "yes" else "no";
      };
    };

    services.netdata.portcheck.checks.openssh.port = (lib.head config.services.openssh.ports);

    networking.firewall.allowedTCPPorts = [ 19999 ];

    environment.etc."netdata/python.d/httpcheck.conf".text = ''
    update_every: 30
    ${lib.concatStringsSep "\n" (mapAttrsToList (site: options:
    ''
      ${site}:
        url: '${options.url}'
        ${optionalString (options.regex != null) "regex: '${options.regex}'"}
        status_accepted: [ ${lib.concatStringsSep " " (map toString options.statusAccepted) } ]
      '') cfg.httpcheck.checks)
      }
    '';

    environment.etc."netdata/python.d/portcheck.conf".text = ''
    ${lib.concatStringsSep "\n" (mapAttrsToList (service: options:
    ''
      ${service}:
        host: '${options.host}'
        port: ${toString options.port}
      '') cfg.portcheck.checks)
      }
    '';
    systemd.services.netdata.restartTriggers = [
      config.environment.etc."netdata/python.d/httpcheck.conf".source 
      config.environment.etc."netdata/python.d/portcheck.conf".source 
      config.environment.etc."netdata/stream.conf".source 
    ];

    # TODO create /etc/netdata
  };

}
