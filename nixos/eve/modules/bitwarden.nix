{ pkgs, config, ... }:
let
  ldapConfig = {
    bitwarden_url = "https://bitwarden.thalheim.io";
    bitwarden_admin_token = "@ADMIN_TOKEN@";
    ldap_host = "localhost";
    ldap_bind_dn = "cn=bitwarden,ou=system,ou=users,dc=eve";
    ldap_bind_password = "@LDAP_PASSWORD@";
    ldap_search_base_dn = "ou=users,dc=eve";
    ldap_search_filter = "(&(objectClass=bitwarden))";
    ldap_sync_interval_seconds = 3600;
  };

  ldapConfigFile = pkgs.runCommand "config.toml"
    {
      buildInputs = [ pkgs.remarshal ];
      preferLocalBuild = true;
    } ''
    remarshal -if json -of toml \
    < ${pkgs.writeText "config.json" (builtins.toJSON ldapConfig)} \
    > $out
  '';
in
{
  services.vaultwarden = {
    enable = true;
    dbBackend = "postgresql";
    config = {
      domain = "https://bitwarden.thalheim.io";
      signupsAllowed = false;
      rocketPort = 3011;
      databaseUrl = "postgresql://vaultwarden@%2Frun%2Fpostgresql/bitwarden_rs";
      enableDbWal = "false";
      websocketEnabled = true;
      smtpHost = "mail.thalheim.io";
      smtpFrom = "bitwarden@thalheim.io";
      smtpUsername = "bitwarden@thalheim.io";
    };
  };

  systemd.services.vaultwarden.serviceConfig = {
    EnvironmentFile = [ config.sops.secrets.bitwarden-smtp-password.path ];
    Restart = "on-failure";
    RestartSec = "2s";
  };

  systemd.services.vaultwarden_ldap = {
    wantedBy = [ "multi-user.target" ];

    preStart = ''
      sed \
        -e "s=@LDAP_PASSWORD@=$(<${config.sops.secrets.bitwarden-ldap-password.path})=" \
        -e "s=@ADMIN_TOKEN@=$(<${config.sops.secrets.bitwarden-admin-token.path})=" \
        ${ldapConfigFile} \
        > /run/vaultwarden_ldap/config.toml
    '';

    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "2s";
      ExecStart = "${pkgs.nur.repos.mic92.vaultwarden_ldap}/bin/vaultwarden_ldap";
      Environment = "CONFIG_PATH=/run/vaultwarden_ldap/config.toml";

      RuntimeDirectory = [ "vaultwarden_ldap" ];
      User = "vaultwarden_ldap";
    };
  };

  services.nginx = {
    virtualHosts."bitwarden.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      extraConfig = ''
        client_max_body_size 128M;
      '';
      locations."/".extraConfig = ''
        proxy_pass http://localhost:3011;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
      '';
      locations."/notifications/hub".extraConfig = ''
        proxy_pass http://localhost:3012;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
      '';
      locations."/notifications/hub/negotiate".extraConfig = ''
        proxy_pass http://localhost:3011;
      '';
    };
  };

  sops.secrets = {
    bitwarden-ldap-password.owner = "vaultwarden_ldap";
    bitwarden-admin-token.owner = "vaultwarden_ldap";
    bitwarden-smtp-password.owner = "vaultwarden";
  };

  users.users.vaultwarden_ldap = {
    isSystemUser = true;
    group = "vaultwarden_ldap";
  };

  users.groups.vaultwarden_ldap = { };
}
