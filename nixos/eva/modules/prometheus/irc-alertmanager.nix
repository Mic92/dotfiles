{ config, pkgs, lib, ... }:
let
  irc-alerts = pkgs.stdenv.mkDerivation {
    name = "irc-alerts";
    src = ./irc-alerts.py;
    dontUnpack = true;
    buildInputs = [ pkgs.python3 ];
    installPhase = ''
      install -D -m755 $src $out/bin/irc-alerts
    '';
  };
in
{
  systemd.sockets =
    lib.mapAttrs'
      (name: opts:
        lib.nameValuePair "irc-alerts-${name}" {
          description = "Receive http hook and send irc message for ${name}";
          wantedBy = [ "sockets.target" ];
          listenStreams = [ "[::]:${builtins.toString opts.port}" ];
        })
      {
        krebs.port = 9223;
      };

  systemd.services =
    lib.mapAttrs'
      (name: opts:
        let
          serviceName = "irc-alerts-${name}";
          hasPassword = opts.passwordFile or null != null;
        in
        lib.nameValuePair serviceName {
          description = "Receive http hook and send irc message for ${name}";
          requires = [ "irc-alerts-${name}.socket" ];
          serviceConfig =
            {
              Environment =
                [
                  "IRC_URL=${opts.url}"
                ]
                ++ lib.optional hasPassword "IRC_PASSWORD_FILE=/run/${serviceName}/password";
              DynamicUser = true;
              User = serviceName;
              ExecStart = "${irc-alerts}/bin/irc-alerts";
            }
            // lib.optionalAttrs hasPassword {
              PermissionsStartOnly = true;
              ExecStartPre =
                "${pkgs.coreutils}/bin/install -m400 "
                + "-o ${serviceName} -g ${serviceName} "
                + "${config.sops.secrets.prometheus-irc-password.path} "
                + "/run/${serviceName}/password";
              RuntimeDirectory = serviceName;
            };
        })
      {
        krebs.url = "irc://prometheus@irc.r:6667/#xxx";
      };
}
