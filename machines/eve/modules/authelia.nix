{
  config,
  pkgs,
  ...
}:
{
  # Authelia as SSO/authentication gateway

  # Vars generator for authelia secrets
  clan.core.vars.generators.authelia = {
    files.jwt-secret = {
      secret = true;
      owner = "authelia-main";
    };
    files.storage-encryption-key = {
      secret = true;
      owner = "authelia-main";
    };
    files.session-secret = {
      secret = true;
      owner = "authelia-main";
    };
    files.ldap-password = {
      secret = true;
      owner = "authelia-main";
    };

    runtimeInputs = with pkgs; [
      coreutils
      openssl
    ];

    script = ''
      gensecret() {
        openssl rand 64 | openssl base64 -A | tr '+/' '-_' | tr -d '='
      }
      # Generate JWT secret (64 random bytes, URL-safe base64)
      # This is also used for password reset identity validation
      gensecret > "$out/jwt-secret"

      # Generate storage encryption key (64 random bytes, URL-safe base64)
      gensecret > "$out/storage-encryption-key"

      # Generate session secret (64 random bytes, URL-safe base64)
      gensecret > "$out/session-secret"

      # Generate LDAP bind password (64 random bytes, URL-safe base64)
      gensecret > "$out/ldap-password"
    '';
  };

  # Authelia service configuration
  services.authelia.instances.main = {
    enable = true;

    # Secret files from clan vars
    secrets = {
      jwtSecretFile = config.clan.core.vars.generators.authelia.files.jwt-secret.path;
      storageEncryptionKeyFile =
        config.clan.core.vars.generators.authelia.files.storage-encryption-key.path;
      sessionSecretFile = config.clan.core.vars.generators.authelia.files.session-secret.path;
    };

    # Pass LDAP and SMTP passwords via environment variables
    environmentVariables = {
      AUTHELIA_AUTHENTICATION_BACKEND_LDAP_PASSWORD_FILE =
        config.clan.core.vars.generators.authelia.files.ldap-password.path;
      AUTHELIA_NOTIFIER_SMTP_PASSWORD_FILE =
        config.clan.core.vars.generators.authelia.files.ldap-password.path;
    };

    settings = {
      default_2fa_method = "totp";

      # Enable password changes and resets
      authentication_backend = {
        password_change.disable = false;
        password_reset.disable = false;
      };

      webauthn = {
        disable = false;
        enable_passkey_login = true;
        display_name = "Authelia";
        attestation_conveyance_preference = "indirect";
        timeout = "60s";
      };

      session = {
        cookies = [
          {
            domain = "thalheim.io";
            authelia_url = "https://auth.thalheim.io";
          }
          {
            domain = "devkid.net";
            authelia_url = "https://auth.devkid.net";
          }
        ];
      };

      storage.postgres = {
        address = "unix:///run/postgresql";
        database = "authelia-main";
        username = "authelia-main";
      };

      notifier.smtp = {
        address = "smtp://mail.thalheim.io:587";
        username = "authelia@thalheim.io";
        sender = "authelia@thalheim.io";
      };

      authentication_backend.ldap = {
        address = "ldap://eve.r:389";
        base_dn = "dc=eve";
        user = "cn=authelia,ou=system,ou=users,dc=eve";
        start_tls = false;

        users_filter = "(&(objectClass=inetOrgPerson)({username_attribute}={input}))";
        additional_users_dn = "ou=users";

        attributes = {
          username = "mail";
          display_name = "cn";
        };

        groups_filter = "(&(objectClass=groupOfNames)(member={dn}))";
        additional_groups_dn = "ou=groups";
      };

      access_control = {
        default_policy = "deny";
        rules = [
          # FreshRSS - restrict to freshrss group members only
          {
            domain = [
              "rss.thalheim.io"
              "rss.devkid.net"
            ];
            policy = "one_factor";
            subject = [ "group:freshrss" ];
          }
          # n8n - restrict to n8n group members only
          {
            domain = "n8n.thalheim.io";
            policy = "one_factor";
            subject = [ "group:n8n" ];
          }
        ];
      };
    };
  };

  # PostgreSQL database for Authelia
  services.postgresql.ensureDatabases = [ "authelia-main" ];
  services.postgresql.ensureUsers = [
    {
      name = "authelia-main";
      ensureDBOwnership = true;
    }
  ];

  # Nginx configuration for Authelia portal
  services.nginx.virtualHosts."auth.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:9091";
      extraConfig = ''
        # Required headers for Authelia
        proxy_set_header X-Original-URL $scheme://$http_host$request_uri;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header X-Forwarded-Host $http_host;
        proxy_set_header X-Forwarded-For $remote_addr;
      '';
    };
  };

  services.nginx.virtualHosts."auth.devkid.net" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:9091";
      extraConfig = ''
        # Required headers for Authelia
        proxy_set_header X-Original-URL $scheme://$http_host$request_uri;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header X-Forwarded-Host $http_host;
        proxy_set_header X-Forwarded-For $remote_addr;
      '';
    };
  };
}
