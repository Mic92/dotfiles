{
  config,
  pkgs,
  self,
  ...
}:
{
  systemd.services.goatcounter = {
    wantedBy = [ "multi-user.target" ];
    enable = true;

    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "2s";
      EnvironmentFile = [ config.clan.core.vars.generators.goatcounter-smtp.files.env.path ];
      ExecStart = ''
        ${
          self.inputs.nur-packages.packages.${pkgs.stdenv.hostPlatform.system}.goatcounter
        }/bin/goatcounter \
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

  clan.core.vars.generators.goatcounter-smtp = {
    files.password = { };
    files.env = { };
    runtimeInputs = [
      pkgs.coreutils
      pkgs.openssl
    ];
    script = ''
      openssl rand -hex 24 > "$out/password"
      printf 'SMTP_PASSWORD=%s' "$(cat "$out/password")" > "$out/env"
    '';
  };

  services.lldap.ensureUsers.goatcounter = {
    email = "goatcounter@thalheim.io";
    passwordFile = config.clan.core.vars.generators.goatcounter-smtp.files.password.path;
    groups = [ "mail" ];
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
    self.inputs.nur-packages.packages.${pkgs.stdenv.hostPlatform.system}.goatcounter
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
