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
      gensecret > "$out/jwt-secret"
      gensecret > "$out/storage-encryption-key"
      gensecret > "$out/session-secret"
      gensecret > "$out/ldap-password"
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
        config.clan.core.vars.generators.authelia.files.ldap-password.path;
    };

    settings = {
      default_2fa_method = lib.mkDefault "totp";

      webauthn = {
        disable = lib.mkDefault false;
        enable_passkey_login = lib.mkDefault true;
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
        base_dn = lib.mkDefault "dc=eve";
        user = lib.mkDefault "cn=authelia,ou=system,ou=users,dc=eve";
        start_tls = lib.mkDefault false;

        users_filter = lib.mkDefault "(&(objectClass=inetOrgPerson)({username_attribute}={input}))";
        additional_users_dn = lib.mkDefault "ou=users";

        attributes = {
          username = lib.mkDefault "mail";
          display_name = lib.mkDefault "cn";
        };

        groups_filter = lib.mkDefault "(&(objectClass=groupOfNames)(member={dn}))";
        additional_groups_dn = lib.mkDefault "ou=groups";
      };

      access_control.default_policy = lib.mkDefault "deny";
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
