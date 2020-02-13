{ lib, ... }:
with lib;
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
}
