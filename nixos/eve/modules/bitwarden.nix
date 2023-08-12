{ pkgs
, config
, inputs
, ...
}:
let
  ldapConfig = {
    vaultwarden_url = "https://bitwarden.thalheim.io";
    vaultwarden_admin_token = "@ADMIN_TOKEN@";
    ldap_host = "localhost";
    ldap_bind_dn = "cn=bitwarden,ou=system,ou=users,dc=eve";
    ldap_bind_password = "@LDAP_PASSWORD@";
    ldap_search_base_dn = "ou=users,dc=eve";
    ldap_search_filter = "(&(objectClass=bitwarden))";
    ldap_sync_interval_seconds = 3600;
  };

  ldapConfigFile =
    pkgs.runCommand "config.toml"
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
      ExecStart = "${inputs.nur-packages.packages.${pkgs.hostPlatform.system}.vaultwarden_ldap}/bin/vaultwarden_ldap";
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
      locations."/" = {
        proxyPass = "http://localhost:3011";
        proxyWebsockets = true;
      };
      locations."/notifications/hub" = {
        proxyPass = "http://localhost:3012";
        proxyWebsockets = true;
      };
      locations."/notifications/hub/negotiate" = {
        proxyPass = "http://localhost:3011";
        proxyWebsockets = true;
      };
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
