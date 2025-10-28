{
  pkgs,
  config,
  self,
  ...
}:
let
  ldapConfig = {
    vaultwarden_url = "https://bitwarden.thalheim.io";
    vaultwarden_admin_token = "@ADMIN_TOKEN@";
    ldap_host = "localhost";
    ldap_bind_dn = "cn=vaultwarden,ou=system,ou=users,dc=eve";
    ldap_bind_password = "@LDAP_PASSWORD@";
    ldap_search_base_dn = "ou=users,dc=eve";
    ldap_search_filter = "(&(memberOf=cn=vaultwarden,ou=users,dc=eve))";
    ldap_sync_interval_seconds = 3600;
  };

  ldapConfigFile =
    pkgs.runCommand "config.toml"
      {
        buildInputs = [ pkgs.remarshal ];
        preferLocalBuild = true;
      }
      ''
        remarshal -if json -of toml \
        < ${pkgs.writeText "config.json" (builtins.toJSON ldapConfig)} \
        > $out
      '';
in
{
  # Vars generator for vaultwarden secrets
  clan.core.vars.generators.vaultwarden = {
    files.admin-token-plaintext = {
      secret = true;
      owner = "vaultwarden_ldap";
    };
    files.admin-token-hash = {
      secret = true;
      owner = "vaultwarden_ldap";
    };
    files.ldap-password = {
      secret = true;
      owner = "vaultwarden_ldap";
    };
    files.smtp-password = {
      secret = true;
      owner = "vaultwarden";
    };

    runtimeInputs = with pkgs; [
      coreutils
      openssl
      libargon2
    ];

    script = ''
      # Generate admin token plaintext (64 random bytes, URL-safe base64)
      openssl rand 64 | openssl base64 -A | tr '+/' '-_' | tr -d '=' > "$out/admin-token-plaintext"

      # Generate random salt for argon2 (16 bytes = 128 bits)
      SALT=$(openssl rand -base64 16 | tr -d '\n')

      # Generate argon2id hash using bitwarden preset: m=64MiB (2^16), t=3, p=4
      # Output format: ADMIN_TOKEN='$argon2id$...'
      HASH=$(echo -n "$(cat "$out/admin-token-plaintext")" | argon2 "$SALT" -id -t 3 -m 16 -p 4 -l 32 -e)
      echo "ADMIN_TOKEN='$HASH'" > "$out/admin-token-hash"

      # Generate LDAP bind password (simple password)
      openssl rand -base64 12 | tr -d '\n' > "$out/ldap-password"

      # Generate SMTP password in environment file format
      echo "SMTP_PASSWORD=$(openssl rand -base64 48 | tr -d '\n')" > "$out/smtp-password"
    '';
  };

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
    EnvironmentFile = [
      config.clan.core.vars.generators.vaultwarden.files.smtp-password.path
      config.clan.core.vars.generators.vaultwarden.files.admin-token-hash.path
    ];
  };

  systemd.services.vaultwarden_ldap = {
    wantedBy = [ "multi-user.target" ];

    preStart = ''
      sed \
        -e "s=@LDAP_PASSWORD@=$(<${config.clan.core.vars.generators.vaultwarden.files.ldap-password.path})=" \
        -e "s=@ADMIN_TOKEN@=$(<${config.clan.core.vars.generators.vaultwarden.files.admin-token-plaintext.path})=" \
        ${ldapConfigFile} \
        > /run/vaultwarden_ldap/config.toml
    '';

    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "2s";
      ExecStart = "${
        self.inputs.nur-packages.packages.${pkgs.hostPlatform.system}.vaultwarden_ldap
      }/bin/vaultwarden_ldap";
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

  users.users.vaultwarden_ldap = {
    isSystemUser = true;
    group = "vaultwarden_ldap";
  };

  users.groups.vaultwarden_ldap = { };
}
