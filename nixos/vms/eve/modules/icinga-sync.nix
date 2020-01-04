{ pkgs, ... }:
{
  systemd.services.icinga-sync = {
    wantedBy = [ "multi-user.target" ];
    path = with pkgs; [ openssh gnused ];
    script = ''
      sed \
        -e "s!@API_PASSWORD@!$(</run/keys/icinga-api-password)!" \
        -e "s!@PUSHOVER_EMAIL@!$(</run/keys/icinga-pushover-email)!" \
        ${./eve.thalheim.io.conf} \
        > eve.thalheim.io.conf
      scp -o 'ProxyJump mic92@fw02.bsd.services' eve.thalheim.io.conf \
        mic92@icingamaster:eve.thalheim.io.conf
      ssh -J mic92@fw02.bsd.services mic92@icingamaster \
        'sudo service icinga2 reload || sudo icinga2 daemon -C'
    '';
    serviceConfig = {
      RuntimeDirectory = "icinga-sync";
      WorkingDirectory = "/run/icinga-sync/";
      Type = "oneshot";
      RemainAfterExit = true;
    };
  };

  krops.secrets.files.icinga-api-password = {};
  krops.secrets.files.icinga-pushover-email = {};
}
