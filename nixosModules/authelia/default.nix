# Shared Authelia configuration
# Import this module and set services.authelia.instances.main.enable = true
{
  config,
  lib,
  pkgs,
  ...
}:
{
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
    runtimeInputs = with pkgs; [
      coreutils
      openssl
    ];

    script = ''
      gensecret() {
        openssl rand 64 | openssl base64 -A | tr '+/' '-_' | tr -d '='
      }
      gensecret > "$out/jwt-secret"
      gensecret > "$out/storage-encryption-key"
      gensecret > "$out/session-secret"
    '';
  };

  # Shared: eva's lldap is a replica of eve's, so both authelia
  # instances must bind with the same credentials.
  clan.core.vars.generators.lldap-authelia = {
    share = true;
    files.bind-password = {
      secret = true;
      owner = "authelia-main";
    };
    runtimeInputs = [
      pkgs.coreutils
      pkgs.openssl
    ];
    script = ''
      openssl rand -base64 24 | tr -d '\n' > "$out/bind-password"
    '';
  };

  services.authelia.instances.main = {
    secrets = {
      jwtSecretFile = config.clan.core.vars.generators.authelia.files.jwt-secret.path;
      storageEncryptionKeyFile =
        config.clan.core.vars.generators.authelia.files.storage-encryption-key.path;
      sessionSecretFile = config.clan.core.vars.generators.authelia.files.session-secret.path;
    };

    environmentVariables = {
      AUTHELIA_AUTHENTICATION_BACKEND_LDAP_PASSWORD_FILE =
        config.clan.core.vars.generators.lldap-authelia.files.bind-password.path;
    };

    settings = {
      default_2fa_method = lib.mkDefault "totp";

      webauthn = {
        disable = lib.mkDefault false;
        enable_passkey_login = lib.mkDefault true;
        # Passkey login with user verification counts as full 2FA, so no
        # second factor prompt is needed after signing in with a passkey.
        experimental_enable_passkey_uv_two_factors = lib.mkDefault true;
        selection_criteria = {
          discoverability = lib.mkDefault "required";
          user_verification = lib.mkDefault "required";
        };
        display_name = lib.mkDefault "Authelia";
        attestation_conveyance_preference = lib.mkDefault "indirect";
        timeout = lib.mkDefault "60s";
      };

      storage.postgres = {
        address = lib.mkDefault "unix:///run/postgresql";
        database = lib.mkDefault "authelia-main";
        username = lib.mkDefault "authelia-main";
      };

      authentication_backend.ldap = {
        implementation = lib.mkDefault "lldap";
        address = lib.mkDefault "ldap://localhost:3890";
        base_dn = lib.mkDefault "dc=eve";
        user = lib.mkDefault "uid=authelia,ou=people,dc=eve";
        start_tls = lib.mkDefault false;

        users_filter = lib.mkDefault "(&(objectClass=person)({username_attribute}={input}))";

        # mail as session username: ACL rules, OIDC subjects and
        # Remote-User headers assume email addresses.
        attributes = {
          username = lib.mkDefault "mail";
          display_name = lib.mkDefault "cn";
        };

        groups_filter = lib.mkDefault "(&(objectClass=groupOfNames)(member={dn}))";
      };

      access_control.default_policy = lib.mkDefault "deny";

      # Dummy rule: in Authelia 4.39 the WebAuthn/passkey registration UI
      # is hidden unless at least one policy requires two_factor. This
      # domain serves nothing; the rule only unlocks the settings panel.
      access_control.rules = [
        {
          domain = "2fa-dummy.thalheim.io";
          policy = "two_factor";
        }
      ];
    };
  };

  # PostgreSQL database for Authelia (only when authelia is enabled)
  services.postgresql = lib.mkIf config.services.authelia.instances.main.enable {
    enable = true;
    ensureDatabases = [ "authelia-main" ];
    ensureUsers = [
      {
        name = "authelia-main";
        ensureDBOwnership = true;
      }
    ];
  };
}
