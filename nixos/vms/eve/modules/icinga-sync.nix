{ pkgs, ... }:
{
  systemd.services.icinga-sync = {
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.openssh ];
    serviceConfig = {
      RuntimeDirectory = "icinga-sync";
      WorkingDirectory = "/run/icinga-sync/";
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = with pkgs; [
        "${coreutils}/bin/install -m644 ${./eve.thalheim.io.conf} eve.thalheim.io.conf"
        "${openssh}/bin/scp -o 'ProxyJump mic92@fw02.bsd.services' eve.thalheim.io.conf \\
          mic92@icingamaster:eve.thalheim.io.conf"
        "${openssh}/bin/ssh -J mic92@fw02.bsd.services mic92@icingamaster \\
          'sudo service icinga2 reload || sudo icinga2 daemon -C'"
      ];
    };
  };
}
