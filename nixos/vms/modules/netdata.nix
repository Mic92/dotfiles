{ config, lib, ... }:

with lib;
{
  options = {
    services.netdata.httpcheck.checks = mkOption {
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
      description = ''
        httpcheck plugin: https://github.com/netdata/netdata/blob/master/collectors/python.d.plugin/httpcheck/httpcheck.conf
      '';
    };
  };
  config = {
    services.netdata = {
      enable = true;
      config = {
        global = {
          "bind to" = (lib.concatStringsSep " " [
            "127.0.0.1:19999"
            "${config.networking.retiolum.ipv4}:19999"
            "[${config.networking.retiolum.ipv6}]:19999"
          ]);
          "error log" = "stderr";
          "update every" = "5";
        };
      };
    };
    networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 19999 ];

    environment.etc."netdata/python.d/httpcheck.conf".text = ''
    ${lib.concatStringsSep "\n" (mapAttrsToList (site: options:
    ''
      ${site}:
        url: '${options.url}'
        ${optionalString (options.regex != null) "regex: '${options.regex}'"}
        status_accepted: [ ${lib.concatStringsSep " " (map toString options.statusAccepted) } ]
      '') config.services.netdata.httpcheck.checks)
      }
    '';
    systemd.services.netdata.restartTriggers = [ config.environment.etc."netdata/python.d/httpcheck.conf".source  ];

    # TODO create /etc/netdata
  };

}
