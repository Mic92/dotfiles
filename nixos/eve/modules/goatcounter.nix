{ pkgs, config, ... }:
{
  systemd.services.goatcounter = {
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "2s";
      EnvironmentFile = [ config.sops.secrets.goatcounter-smtp-password.path ];
      ExecStart = ''
        ${pkgs.nur.repos.mic92.goatcounter}/bin/goatcounter \
          -automigrate \
          -listen localhost:3003 \
          -tls none \
          -db postgres://dbname=goatcounter \
          -smtp smtp://goatcounter@thalheim.io:''${SMTP_PASSWORD}@mail.thalheim.io
      '';
      User = "goatcounter";
    };
  };

  services.postgresql.ensureDatabases = [ "goatcounter" ];
  services.postgresql.ensureUsers = [{
    name = "goatcounter";
    ensurePermissions."DATABASE goatcounter" = "ALL PRIVILEGES";
  }];

  sops.secrets.goatcounter-smtp-password = {};

  users.users.goatcounter = {
    isSystemUser = true;
    group = "goatcounter";
  };
}
