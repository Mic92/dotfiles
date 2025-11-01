{ config, pkgs, ... }:
{
  imports = [ ../../../nixosModules/phpldapadmin ];

  services.phpldapadmin = {
    enable = true;
    domain = "ldap.thalheim.io";

    # Generated via clan.core.vars.generators
    appKey = config.clan.core.vars.generators.phpldapadmin.files.app-key.path;

    # Use local PostgreSQL database
    database.createLocally = true;

    # LDAP connection to local OpenLDAP
    ldap = {
      host = "localhost";
      port = 389;
      baseDn = "dc=eve";
      useSsl = false;
      useTls = false;
      loginAttr = "mail"; # Allow login with email address
      allowGuest = false;
      bindDn = "cn=phpldapadmin,ou=system,ou=users,dc=eve";
      bindPasswordFile = config.clan.core.vars.generators.phpldapadmin.files.bind-password.path;
    };

    nginx.enable = true;

    templates = {
      custom.user_with_mail = {
        title = "User Account with Email";
        description = "New User Account with Email Access and Password";
        enabled = true;
        icon = "fa-user-circle";
        rdn = "cn";
        regexp = "/^ou=.+,?/";
        objectclasses = [
          "inetOrgPerson"
          "simpleSecurityObject"
          "mailAccount"
          "top"
        ];
        attributes = {
          givenName = {
            display = "First Name";
            onchange = [
              "=autoFill(cn;%givenName% %sn/U%)"
              "=autoFill(uid;%givenName|0-1/l%%sn/l%)"
            ];
            order = 1;
          };
          sn = {
            display = "Last Name";
            onchange = [
              "=autoFill(cn;%givenName% %sn/U%)"
              "=autoFill(uid;%givenName|0-1/l%%sn/l%)"
            ];
            order = 2;
          };
          cn = {
            display = "Common Name";
            readonly = true;
            order = 3;
          };
          uid = {
            display = "User ID";
            onchange = [
              "=autoFill(mail;%uid%@)"
            ];
            order = 4;
          };
          mail = {
            display = "Email Address";
            order = 5;
          };
          userPassword = {
            display = "Password";
            helper = "ARGON2ID";
            order = 6;
          };
        };
      };
    };

    poolSettings = {
      "pm" = "dynamic";
      "pm.max_children" = 16;
      "pm.start_servers" = 2;
      "pm.min_spare_servers" = 2;
      "pm.max_spare_servers" = 4;
      "pm.max_requests" = 500;
    };
  };

  # Add SSL configuration
  services.nginx.virtualHosts."ldap.thalheim.io" = {
    forceSSL = true;
    enableACME = true;
  };

  # Generate Laravel app key and LDAP bind password using clan vars generator
  clan.core.vars.generators.phpldapadmin = {
    files = {
      app-key = {
        owner = "phpldapadmin";
      };
      bind-password = {
        owner = "phpldapadmin";
      };
    };
    runtimeInputs = [
      pkgs.openssl
      pkgs.coreutils
    ];
    script = ''
      # Generate Laravel app key in the correct format (base64:32-bytes)
      # Note: tr strips the trailing newline from openssl output
      printf "base64:%s" "$(openssl rand -base64 32 | tr -d '\n')" > "$out"/app-key

      # Generate bind password (32 random bytes)
      openssl rand -base64 32 | tr -d '\n' > "$out"/bind-password
    '';
  };
}
