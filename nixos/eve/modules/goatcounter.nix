{ config, pkgs, inputs, ... }: {
  systemd.services.goatcounter = {
    wantedBy = [ "multi-user.target" ];
    enable = true;

    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "2s";
      EnvironmentFile = [ config.sops.secrets.goatcounter-smtp-password.path ];
      ExecStart = ''
        ${inputs.nur-packages.packages.${pkgs.hostPlatform.system}.goatcounter}/bin/goatcounter \
          serve \
          -automigrate \
          -listen localhost:3004 \
          -tls none \
          -db 'postgresql+host=/run/postgresql dbname=goatcounter sslmode=disable' \
          -smtp smtp://goatcounter@thalheim.io:''${SMTP_PASSWORD}@mail.thalheim.io
      '';
      User = "goatcounter";
    };
  };

  services.nginx.virtualHosts."goatcounter.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://localhost:3004;
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Proto $scheme;
    '';
  };

  environment.systemPackages = [
    inputs.nur-packages.packages.${pkgs.hostPlatform.system}.goatcounter
  ];

  services.postgresql.ensureDatabases = [ "goatcounter" ];
  services.postgresql.ensureUsers = [
    {
      name = "goatcounter";
      ensureDBOwnership = true;
    }
  ];

  users.users.goatcounter = {
    isSystemUser = true;
    group = "goatcounter";
  };
  users.groups.goatcounter = { };
}
